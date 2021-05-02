library(tidyr)

var_names<-dput(names(data))


questions <- c(NA, NA, NA, NA, NA, NA, "Which of these best describes you?", 
               "Are you currently in a committed romantic relationship?", 
               "Have you ever been in a committed romantic relationship?", 
               "Are you currently casually dating anyone?", "Which of these best describes what you are looking for right now?", 
               "Please indicate if the following is a reason or not a reason why you are not currently looking for a relationship or dates", 
               "Too busy", "Haven't had luck with dating or relationships in the past", "Just like being single", 
               "Have health problems that make it difficult to date", 
               "Have more important priorities right now", "Not ready to date after ending a relationship", 
               "Feel like I am too old to date", "Feel like no one would be interested in dating me",  
               "How long have you been in your current romantic relationship?", "Overall, would you say that things in your relationship are going...",
               "Overall, would you say that things in your dating life are going...", "Have you ever used an online dating site or dating app?",
               "Are you currently using an online dating site or dating app?", "How did you first meet your spouse or partner?", 
               "Where online did you first meet your spouse or partner?", "Compared to 10 years ago, for most people, do you think dating is...", 
               "Why do you think dating is harder today than it was 10 years ago?", "Why do you think dating is easier today than it was 10 years ago for most people?", 
               "Here is a list of things that some people may find acceptable ON A FIRST DATE and some may not. Regardless of whether you would do it yourself, please indicate if you, personally, think each is acceptable on a first date.",
               "Giving a hug", "Kissing", "Having sex", "Here is a list of things that some people may find acceptable and some may not. Regardless of whether you would do it yourself, please indicate if you, personally, think each is acceptable.", 
               "Sex between unmarried adults who are in a committed relationship", "Having an open relationship- that is, a committed relationship where both people agree that it is acceptable to date or have sex with other people", 
               "Casual sex between consenting adults who are not in a committed relationship", "Two consenting adults exchanging sexually explicit images of themselves", 
               "Kissing someone on a date without asking permission first", "Have you ever searched for information online about someone you were romantically interested in?", 
               "Regardless of whether you would do it yourself, do you think it’s ever acceptable for someone to look through their significant other’s cellphone without their knowledge?", 
               "If you decided after a first date that you didn’t want to go out with that person again, what is the most likely way you would let them know?", 
               "Whether or not you would personally do this, do you think it’s acceptable for a person to break up with someone they are casually dating in the following ways?", 
               "In person", "Through a phone call", "Through email", "Through a text message", 
               "Whether or not you would personally do this, do you think it’s acceptable for a person to break up with someone they are in a committed relationship with in the following ways?",
               "In person", "Through a phone call", "Through email", "Through a text message", "Do you think the increased focus on sexual harassment and assault over the last few years has made it easier or harder for MEN to know how to interact with someone they’re on a date with?",
               "Do you think the increased focus on sexual harassment and assault over the last few years has made it easier or harder for WOMEN to know how to interact with someone they’re on a date with?",
               "Overall, what type of effect would you say online dating sites and dating apps have had on dating and relationships?",
               "What is the main reason you think online dating sites and dating apps have a mostly positive effect on dating and relationships?",
               "What is the main reason you think online dating sites and dating apps have a mostly negative effect on dating and relationships?",
               "Compared to relationships that begin in person, in general, do you think relationships where people first meet through an online dating site or dating app are…",
               "In general, how safe do you think online dating sites and dating apps are as a way to meet people?",
               "Thinking of some things that may happen on online dating sites and dating apps, as far as you know, how common are each of the following?",
               "People being harassed or bullied", "People receiving sexually explicit messages or images they did not ask for", "People lying about themselves to appear more desirable",
               "Privacy violations, such as data breaches or identity theft", "People setting up fake accounts in order to scam others",
               "Would you ever consider being in a committed relationship with someone who…", "Is of a different religion than you", "Is of a different race or ethnicity than you", "Has a significant amount of debt",
               "Is raising children from another relationship", "Lives far away from you", "Would you ever consider being in a committed relationship with someone who…",
               "Is a Republican", "Is a Democrat", "Makes significantly more money than you", "Makes significantly less money than you", 
               "Would you ever consider being in a committed relationship with someone who…", "Voted for Donald Trump", "Voted for Hillary Clinton", 
               "Is 10 years older than you", "Is 10 years younger than you", "How much pressure, if any, do you feel from each of the following to be in a committed relationship?",
               "Family members", "Your friends", "Society", "In the past year, how easy or difficult has it been for you to find people to date?", "Why do you think it has been difficult for you to find people to date?", 
               "There is a limited number of people in my area for me to date", "Hard for me to find someone who meets my expectations", "Hard to find someone who's looking for the same type of relationship as me", 
               "Hard for me to approach people", "People aren't interested in dating me", "Too busy", "Overall, would you say your OWN personal experiences with online dating sites or dating apps have been…",
               "In general in the past year, has using online dating sites or dating apps made you feel more…",
               "1 Confident
              2 Insecure
               3 Neither", "1 Optimistic
              2 Pessimistic
               3 Neither", "1 Hopeful
                2 Frustrated
               3 Neither", "Have you ever...", "Gone on a date with someone you met through an online dating site or dating app", "Been in a committed relationship or married someone you first met through an online dating site or dating app",
               "Come across the online dating profile of someone you already know offline", "When you have used online dating sites or dating apps, how important was it to you, if at all, that the profiles you looked at included the following?",
               "Hobbies and interests", "Political affiliation", "Religious beliefs", "Occupation", "Racial or ethnic background", "Height", "If they have children", "Type of relationship they're looking for", "Photos of themselves",
               "When you used online dating sites or dating apps, how easy or difficult was it for you to find people on online dating sites or dating apps who…", "You were physically attracted to", "Shared your hobbies and interests", "Were looking for the same kind of relationship as you",
               "Seemed like someone you would want to meet in person", "Thinking about the messages you have received on online dating sites or dating apps, would you say you have received…",
               "Thinking about the messages you have received on online dating sites or dating apps, would you say you have received…", "How well, if at all, do you feel you understand why online dating sites or dating apps present certain people as potential matches for you?",
               "How concerned are you, if at all, about how much data online dating sites or dating apps collect about you?","Do you ever use social media sites, like Facebook, Twitter, or Instagram?",
               "How often, if ever, do you see people posting things about their romantic relationships on social media?", "In general, do the posts you see on social media about other people’s romantic relationships make you feel…",
               "In general, do the posts you see on social media about other people’s romantic relationships make you feel…", "Have you ever used social media to do any of the following things?",
               "Check up on someone that you used to date or be in a relationship with",
               "Share or discuss things about your relationship or dating life","As far as you know, does your spouse or partner…","Have a cellphone","Use social media sites","Play video games on a computer, game console or cellphone","How important, if at all, is social media to you personally when it comes to the following things?",
               "Keeping up with what's going on your spouse's or partner's life", "Showing how much you care about your spouse or partner","Have you ever felt jealous or unsure about your relationship because of the way your current spouse or partner interacts with other people on social media?",
               "How often, if ever, do you feel as if your spouse or partner is distracted by their cellphone when you are trying to have a conversation with them?",
               "How often, if ever, are you bothered by the amount of time your spouse or partner spends…", "On their cellphone", "On social media sites", "Playing video games", "Have you ever given your spouse or partner the password or passcode to...", "Your email account", "Any of your social media accounts", "Your cellphone",
               "Have you ever looked through your current spouse's or partner's cellphone without their knowledge?", "Have you ever heard of the following terms associated with dating?", "Ghosting", "Breadcrumbing", "Phubbing", "Catfishing", "Friends with benefits", "Have you ever had someone you’ve gone out with suddenly stop answering your phone calls or messages without explanation (sometimes called “ghosting”)?",
               "Has someone you were dating or on a date with ever done any of the following?",
               "Pressured you for sex", "Touched you in a way that made you feel uncomfortable", "Sent you sexually explicit images that you didn't ask for", "As far as you know, has someone you were dating or been on a date with ever done any of the following?",
               "Spread rumors about your sexual history", "Shared a sexually explicit image of you without your consent", "Publically shared your contact information or address without your permission", 
               "Thinking about your own personal experiences, has someone ever done any of the following to you ON AN ONLINE DATING SITE OR DATING APP?","Called you an offensive name","Threatened to physically harm you","Sent you a sexually explicit message or image you didn’t ask for","Continued to contact you after you said you were not interested",
               "Now thinking about your spouse or partner, are they...",
               "Do you think of yourself as...", NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA, NA)
            
          
 
lookup_questions <- data.frame(var_names, questions)

