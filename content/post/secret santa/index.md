---
authors:
- admin
categories: []
date: "2019-12-01T00:00:00Z"
draft: false
featured: false
image:
  caption: ""
  focal_point: ""
lastMod: "2019-12-01T00:00:00Z"
projects: []
subtitle: A festivus for the rest of us!
summary: We talk a bit about a brief example of a Secret Santa program!
tags: [R, Rstats]
title: Creating a Secret Santa program in R
---

## The Inspiration

My friends and I started a tradition shortly after we left college. Despite being spread out across the country (and now, the world!), we wished to remain in touch and get each other some holiday cheer. As the years moved on, and as we added to our number with partners (and now offspring!), we wanted to continue the tradition of secret santa. One year, my buddy [Alex](www.alexrecker.com) created a python secret santa bot that divvied us up and sent out emails to all parties involved. This served as the inspation for this project: I wanted to create a program that could divvy us up without giving away our information to others as well as teach myself some programming in the process.

![](https://media.giphy.com/media/wranrCRq3f90A/source.gif)

While writing this little program, I found a lot of great ideas and code snippets from other useRs who looked at these very issues including: [Tristan Mahr's graphing example](https://www.tjmahr.com/secret-santa-graph-traversal/), [Sarah Lotspeich's and Lucy D'Agostino McGowan's example and their ponyexpress integration ](https://livefreeordichotomize.com/2017/11/15/secret-sampling/), and [David Selby's secret santa example](https://selbydavid.com/2016/12/07/santa/). Thanks so much all for helping me create my real first program! 

## The problem

We wanted a program that could do a couple of things:

1. Match up santas and receivers that were not partners (i.e., in the same family)
2. Once the list has been created, email out the results to everyone so that whoever hosted it did not know who their secret santa was!

## A solution?

So, I started to attempt to plan out a game plan to tackle these two specific problems. However, before doing this, I created a test data frame because I wanted to make sure to test the program as I went along.

```
#Create a test data frame
santa_firstnames <- c("Jack", "Jill", "Lyle", "Lilly", "Bill", "Dan", "Deloris", "Emily")
santa_lastnames <- c("Smith", "Smith", "Lewis", "Lewis", "Buttes", "Deznada", "Deznada", "Richardson")
santa_email <- c("jacks@test.com", "jills@test.com", "lylel@test.com", "lillyl@test.com", "billb@test.com",
           "dand@test.com","delorisd@test.com", "emilyr@test.com")
santa_address <- c("123 Cherryhill Drive, San Diego, CA 92103","123 Cherryhill Drive, San Diego, CA 92103",
                   "234 elms ln, Anchorage, AK 99116", "234 Carita ln, Anchorage, AK 99116",
                   "111 Pleasantview Dr., Chicago 11111", "222 ELM St., Atlanta, GA 22222",
                   "222 ELM St., Atlanta, GA 22222", "999 Peaches St., Tokyo, JPN, 11111")
testdf <- as.data.frame(cbind(santa_firstnames,santa_lastnames,santa_email,santa_address))
```

This dataframe would serve as my testing agent throughout the process.

The first function I used was sample():

```
  newdf <- testdf %>%
    mutate(receiver_firstnames = sample(santa_firstnames))
    
```

I then changed the variable names on the original dataframe and pulled them into this newly created dataframe.

```
  #Create a partner data frame to merge partner variables into it!
  partnerdf <- testdf %>%
    transmute(receiver_firstnames = santa_firstnames,
              receiver_lastnames = santa_lastnames,
              receiver_email = santa_email,
              receiver_address = santa_address)
  #Now join the data frames!
  newdf <- dplyr::inner_join(newdf,partnerdf, by = "receiver_firstnames") 
```

Although I knew using the sample() function could be helpful, I soon ran into the problem of having an individual being assigned to themselves. In order to avoid a Kevin scenario [GIF OF KEVIN INSERT], we had to add an ifelse() statement!

```
newdf$self_eval <- ifelse(newdf$receiver_firstnames == newdf$santa_firstnames, "Problems!", "No problem here!")

```
This did not necessarily change anything, but it let me knew if there was an issue where someone had themselves!

I did something similar for last names to let me know if there was an issue.

```
  newdf$fam_eval <- ifelse(newdf$receiver_lastnames == newdf$santa_lastnames, "Problems!", "No problem here!")
```

When I got here, I was pretty satisfied! It took a few times of me pressing buttons to rerun the code, but usually after a few tries, I would get a data frame that had no problems!

However, I am quite lazy, and did not wish to continue to press the button. This is probably the weaker area of coding for me, but I needed to have the program continue to iterate until an adequate solution was found. Enter the repeat loop with a condition statement!

```
#Create a repeat loop that will continue to go over this list until there are no issues (1. No person has themselves. 2. No person has their partner.)
repeat {
...
  #Final problem check!
  newdf$finaleval <- ifelse(newdf$fam_eval == "Problems!" | newdf$self_eval == "Problems!",print("Stop, there are issues!"),print("Ain't no issues here!"));
  #Drop superflous columns and create output dataframe!
  SecretSanta <- newdf %>% select(-c(receiver_email,santa_address,fam_eval,self_eval,finaleval))
  if (all(newdf$finaleval == "Ain't no issues here!")) { #Are there issues? If so, repeat loop.
    print("We are all finished with divvying up the presents!");
    break
  }
}
```
I created a final evaluation check that examined our two above error statments, and if there were some problems, it would let us know, but then would repeat the process until a resultant dataframe was created which satisfied all of our conditions.

I then also had it create a final dataframe that was a little bit cleaner for our final step in the process: letting individuals know whom to buy for!

To do this, I slightly adapted the [Ponyexpress Package](https://github.com/ropenscilabs/ponyexpress). For some reason, I got an error when it was validating emails, and so I took that portion of the function out (I am not too familiar with grepl and associated functions or stringr), and then the code worked pretty well.

```
parcel_create <- function(df,
                          sender_name = NULL,
                          sender_email = NULL,
                          subject = NULL,
                          bcc = NULL,
                          template = NULL) {
  emails <- NULL
  if (is.null(df) || is.null(sender_name) || is.null(sender_email) || is.null(template)) {
    stop("You must supply a value for: df, sender_name, sender_email, and template")
  }
  
  email <- df
  email$To <- glue::glue_data(df,"{santa_firstnames} <{santa_email}>")
  email$Bcc <- bcc
  email$From <- glue::glue("{sender_name} <{sender_email}>")
  email$Subject <- subject
  email$body <- glue::glue_data(df, template)
  email <- email[, names(email) %in% c("To", "Bcc", "From", "Subject", "body")]
  structure(email, class = c("parcel", "data.frame"))
}
```

We then create our nice little body of text with our santa message and the GIF from above to spread some holiday cheer:

```

body <- "Merry Christmas Santa {santa_firstnames}!

In the spirit of Santa, you will be getting a gift for {receiver_firstnames}. 

Make sure to get it to {receiver_address} by Dec 25!


<img src = 'https://media.giphy.com/media/zhPXoVIBMtnUs/giphy.gif'> </img>

Santa R Bot"

our_template <- glue::glue(glitter_template)

```
Once we are done with this and creating our template, we create the parcel!

```
parcel <- parcel_create(SecretSanta,
                        sender_name = "Jordan",
                        sender_email = "testemail@test.com",
                        subject = "SECRET SANTA",
                        template = our_template)

parcel_preview(parcel)  

parcel_send(parcel)
```

This is really an excellent package that I hope to use more in the future! I don't know what part of my email was causing the validation error, but taking it out seemed to help.

Then, the emails were sent!

## Critique

I doubt that this is the most efficient or elegant way to solve this issue. I would prefer a method a bit more proactive rather than bruteforcing an approach, but I am quite pragmatic at the end of the day. This way of solving may result in closed loops of folks giving to each other (e.g., Person A gifts to Person B who gifts to Person C who gifts to Person A while we have three other people that give to each other without crossover), but this is not something we particularly care about at this point in time. I don't know how this process would scale with more folks, but this is good enough for where we are at, I think.

## Reflections on the process

When I began using R, I was trying to finish my dissertation with it, which I did, but it was a mess. I feel like I have come far from that point. However, going through this process I learned that I have a ways to go. Yet, once I got that repeat loop working, I felt proud of what I accomplished. As my partner could tell you once I sent the draft emails, I was elated! It felt awesome to solve a small problem.

As I look to more analytic projects in the future, I think I took away some helpful tips. First and foremost, sitting down to write out the steps of the problem as I saw them and planning was critical. It kept me on task, kept the tasks more manageable, and allowed me to compartmentalize testing. Testing the program (albeit crude) along the way to make sure the steps were working as I intended and then testing the program with the steps I completed to at that point helped me catch so many errors, big and small, while my thinking was still fresh. Third, was to have some good food while doing it and watch some football!

Would love to have some feedback!

Jordan