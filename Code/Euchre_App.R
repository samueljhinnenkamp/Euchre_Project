library(shiny)
library(glue)
library(dplyr)
library(rms)
library(tidytext)
library(nnet)
library(DT)

# Create UI
ui = fluidPage(
  tags$style(HTML("
    .column-name {
      text-align: center;
      color: white;
      font-weight: bold;
      background-color: red; /* Green background */
      padding: 10px 0;
    }
    .input-column {
      border: 1px solid #ccc; /* Optional border for visibility */
      padding: 10px;
    }
    .rec-text {
      font-weight: bold;
      font-size: 24px;
      color: black;
    }
  ")),
  
  titlePanel("Euchre Hand Evaluation"),
  
  p("Enter the cards of your dealt hand as well as some other information about your Euchre round."),
  
  tags$div(style = "height: 5px;"),
  
  fluidRow(
    column(2, div(class = "column-name", "Card 1"), style = "margin-right: 25px;"),
    column(2, div(class = "column-name", "Card 2"), style = "margin-right: 25px;"),
    column(2, div(class = "column-name", "Card 3"), style = "margin-right: 25px;"),
    column(2, div(class = "column-name", "Card 4"), style = "margin-right: 25px;"),
    column(2, div(class = "column-name", "Card 5"), style = "margin-right: 25px;")
  ),
  
  fluidRow(
    column(2, div(class = "input-column", selectInput("Card1value", "Value", choices = c(" ", "Ace", "King", "Queen", "Jack", "Ten", "Nine"))), style = "margin-right: 25px;"),
    column(2, div(class = "input-column", selectInput("Card2value", "Value", choices = c(" ", "Ace", "King", "Queen", "Jack", "Ten", "Nine"))), style = "margin-right: 25px;"),
    column(2, div(class = "input-column", selectInput("Card3value", "Value", choices = c(" ", "Ace", "King", "Queen", "Jack", "Ten", "Nine"))), style = "margin-right: 25px;"),
    column(2, div(class = "input-column", selectInput("Card4value", "Value", choices = c(" ", "Ace", "King", "Queen", "Jack", "Ten", "Nine"))), style = "margin-right: 25px;"),
    column(2, div(class = "input-column", selectInput("Card5value", "Value", choices = c(" ", "Ace", "King", "Queen", "Jack", "Ten", "Nine"))), style = "margin-right: 25px;")
  ),

  fluidRow(
    column(2, div(class = "input-column", selectInput("Card1suit", "Suit", choices = c(" ", "Clubs", "Diamonds", "Hearts", "Spades"))), style = "margin-right: 25px;"),
    column(2, div(class = "input-column", selectInput("Card2suit", "Suit", choices = c(" ", "Clubs", "Diamonds", "Hearts", "Spades"))), style = "margin-right: 25px;"),
    column(2, div(class = "input-column", selectInput("Card3suit", "Suit", choices = c(" ", "Clubs", "Diamonds", "Hearts", "Spades"))), style = "margin-right: 25px;"),
    column(2, div(class = "input-column", selectInput("Card4suit", "Suit", choices = c(" ", "Clubs", "Diamonds", "Hearts", "Spades"))), style = "margin-right: 25px;"),
    column(2, div(class = "input-column", selectInput("Card5suit", "Suit", choices = c(" ", "Clubs", "Diamonds", "Hearts", "Spades"))), style = "margin-right: 25px;")
  ),

  tags$div(style = "height: 20px;"),
  
  fluidRow(
    column(5, div(class = "column-name", "Who is the Dealer?"), style = "margin-right: 100px;"),
    column(5, div(class = "column-name", "Turned Up Card"))
    ),
  
  fluidRow(
    column(5, div(class = "input-column", selectInput("Dealer", "Dealer", choices = c(" ", "Me", "My Partner", "Opponent to left (I DO NOT deal next)", "Opponent to right (I deal next)"))), style = "margin-right: 100px;"),
    column(5, div(class = "input-column", selectInput("TurnedUpvalue", "Value", choices = c(" ", "Ace", "King", "Queen", "Jack", "Ten", "Nine"))))
  ),

  fluidRow(
    column(5, style = "margin-right: 100px;"),
    column(5, div(class = "input-column", selectInput("TurnedUpsuit", "Suit", choices = c(" ", "Clubs", "Diamonds", "Hearts", "Spades"))))
  ),
  
  actionButton(inputId = "Predict", label = "Analyze Hand", style = "font-size: 20px; padding: 10px 20px; width: 200px; height: 50px; background-color: #f0f0f0"),

  tags$div(style = "height: 25px;"),
  
  tags$div(
    class = "rec-text",
    textOutput("RecText")
  ),
  
  tags$div(style = "height: 10px;"),
  
  textOutput("AdlRecText"),
  
  tags$div(style = "height: 30px;"),
  
  textOutput("myExplanation"),
  
  tags$div(style = "height: 10px;"),
  
  DTOutput("RecTable"), 
  
  tags$div(style = "height: 30px;"),
  
  uiOutput("Reset_Button"),
  
  tags$div(style = "height: 30px;")
  
  )


server = function(input, output) {
  observeEvent(input$Predict, {
    
    # Read in models
    euchre_turn_up_model = readRDS('euchre_turn_up_model.RDS')
    euchre_turn_down_model = readRDS('euchre_turn_down_model.RDS')
    
    # Initially define outputs as NULL
    note = NULL
    explanation = NULL
    bid_probability_table = NULL
    
    
    # Create 5 dataframes, each representing a card from the hand
    card1 = data.frame()
    card2 = data.frame()
    card3 = data.frame()
    card4 = data.frame()
    card5 = data.frame()
    
    card1[1,1]$value = input$Card1value
    card1[1,2]$suit = input$Card1suit
    card2[1,1]$value = input$Card2value
    card2[1,2]$suit = input$Card2suit
    card3[1,1]$value = input$Card3value
    card3[1,2]$suit = input$Card3suit
    card4[1,1]$value = input$Card4value
    card4[1,2]$suit = input$Card4suit
    card5[1,1]$value = input$Card5value
    card5[1,2]$suit = input$Card5suit
    
    cards = rbind(card1, card2, card3, card4, card5) # Combine cards to make hand
    
    # Create turned up card
    turned_up = data.frame()
    turned_up[1,1]$value = input$TurnedUpvalue
    turned_up[1,2]$suit = input$TurnedUpsuit
    
    everything = rbind(cards, turned_up)
    
    dealer = input$Dealer
    
    # Additional objects to be included as fields
    pick_up_scenarios = NULL
    pick_up_scenario = NULL
    
    # Check to ensure entries are logically feasible and all fields are populated
    if(input$Card1value == ' ' |
              input$Card2value == ' ' |
              input$Card3value == ' ' |
              input$Card4value == ' ' |
              input$Card5value == ' ' |
              input$Card1suit == ' ' |
              input$Card2suit == ' ' |
              input$Card3suit == ' ' |
              input$Card4suit == ' ' |
              input$Card5suit == ' ' |
              input$TurnedUpvalue == ' '|
              input$TurnedUpsuit == ' '|
              input$Dealer == ' '){
      recommendation = 'Your entry is incomplete. Please ensure you have entered something in all fields.'
      output$Reset_Button = renderUI({})
    } else if(nrow(distinct(everything)) != 6){
      recommendation = 'This card combination is impossible to have. You entered a card in multiple times.'
      output$Reset_Button = renderUI({})
    }
    else{
      # Identify cards that are trump for each suit
      cards$Hearts_trump = ifelse(cards$suit == 'Hearts' | (cards$value == 'Jack' & cards$suit == 'Diamonds'), 1, 0)
      cards$Diamonds_trump = ifelse(cards$suit == 'Diamonds' | (cards$value == 'Jack' & cards$suit == 'Hearts'), 1, 0)
      cards$Clubs_trump = ifelse(cards$suit == 'Clubs' | (cards$value == 'Jack' & cards$suit == 'Spades'), 1, 0)
      cards$Spades_trump = ifelse(cards$suit == 'Spades' | (cards$value == 'Jack' & cards$suit == 'Clubs'), 1, 0)
      
      # Logic that determines the start order, based on provided dealer information
      p_picked_up = 'None'
      opp_picked_up = 'None'
      
      if(dealer == 'Me'){
        Broad_Start_Order = 'Last'
      } else if(dealer == 'My Partner'){
        Broad_Start_Order = 'Middle'
        if(turned_up$value == 'Jack'){
          p_picked_up = 'Jack'
        } else{
          p_picked_up = 'Other'
        }
      } else if(dealer == "Opponent to left (I DO NOT deal next)"){
        Broad_Start_Order = 'Middle'
        if(turned_up$value == 'Jack'){
          opp_picked_up = 'Jack'
        } else{
          opp_picked_up = 'Other'
        }
      } else if(dealer == "Opponent to right (I deal next)"){
        Broad_Start_Order = 'First'
        if(turned_up$value == 'Jack'){
          opp_picked_up = 'Jack'
        } else{
          opp_picked_up = 'Other'
        }
      }
      
      suits = c('Hearts', 'Diamonds', 'Clubs', 'Spades')
      
      
      # If I'm the dealer, assess model performance when picking up the turned up card and discarding each of the 5 cards. The hand with the best model performance determines which card gets discarded if a pick up happens. Tiebreakers are randomly done 
      if(dealer == 'Me'){
        turned_up$Hearts_trump = ifelse(turned_up$suit == 'Hearts' | (turned_up$value == 'Jack' & turned_up$suit == 'Diamonds'), 1, 0)
        turned_up$Diamonds_trump = ifelse(turned_up$suit == 'Diamonds' | (turned_up$value == 'Jack' & turned_up$suit == 'Hearts'), 1, 0)
        turned_up$Clubs_trump = ifelse(turned_up$suit == 'Clubs' | (turned_up$value == 'Jack' & turned_up$suit == 'Spades'), 1, 0)
        turned_up$Spades_trump = ifelse(turned_up$suit == 'Spades' | (turned_up$value == 'Jack' & turned_up$suit == 'Clubs'), 1, 0)
        pick_up_scenarios = data.frame()
        suit = turned_up$suit
        for(i in 1:nrow(cards)){
          cards_scenario = cards
          discarded = cards_scenario[i,]
          cards_scenario = cards_scenario[-i,]
          cards_scenario = rbind(cards_scenario, turned_up)
          pick_up_scenario_iter = data.frame()
          pick_up_scenario_iter[1,1]$Right_Bower = nrow(cards_scenario[cards_scenario$value == 'Jack' & cards_scenario$suit == suit & cards_scenario[glue('{suit}_trump')] == 1,])
          pick_up_scenario_iter[1,2]$Left_Bower = nrow(cards_scenario[cards_scenario$value == 'Jack' & cards_scenario$suit != suit & cards_scenario[glue('{suit}_trump')] == 1,])
          pick_up_scenario_iter[1,3]$TAce = nrow(cards_scenario[cards_scenario$value == 'Ace' & cards_scenario$suit == suit, ])
          pick_up_scenario_iter[1,4]$TKing = nrow(cards_scenario[cards_scenario$value == 'King' & cards_scenario$suit == suit, ])
          pick_up_scenario_iter[1,5]$TQueen = nrow(cards_scenario[cards_scenario$value == 'Queen' & cards_scenario$suit == suit, ])
          pick_up_scenario_iter[1,6]$TTen = nrow(cards_scenario[cards_scenario$value == 'Ten' & cards_scenario$suit == suit, ])
          pick_up_scenario_iter[1,7]$TNine = nrow(cards_scenario[cards_scenario$value == 'Nine' & cards_scenario$suit == suit, ])
          pick_up_scenario_iter[1,8]$Ace_count = nrow(cards_scenario[cards_scenario$value == 'Ace' & cards_scenario[glue('{suit}_trump')] != 1,])
          pick_up_scenario_iter[1,9]$King_count = nrow(cards_scenario[cards_scenario$value == 'King' & cards_scenario[glue('{suit}_trump')] != 1,])
          pick_up_scenario_iter[1,10]$Queen_count = nrow(cards_scenario[cards_scenario$value == 'Queen' & cards_scenario[glue('{suit}_trump')] != 1,])
          pick_up_scenario_iter[1,11]$Jack_count = nrow(cards_scenario[cards_scenario$value == 'Jack' & cards_scenario[glue('{suit}_trump')] != 1,])
          pick_up_scenario_iter[1,12]$Ten_count = nrow(cards_scenario[cards_scenario$value == 'Ten' & cards_scenario[glue('{suit}_trump')] != 1,])
          pick_up_scenario_iter[1,13]$Nine_count = nrow(cards_scenario[cards_scenario$value == 'Nine' & cards_scenario[glue('{suit}_trump')] != 1,])
          pick_up_scenario_iter[1,14]$Count_Trump = nrow(cards_scenario[cards_scenario[glue('{suit}_trump')] == 1,])
          pick_up_scenario_iter[1,15]$Non_Trump_Suit_Count = length(unique(cards_scenario[cards_scenario[glue('{suit}_trump')] != 1,]$suit))
          pick_up_scenario_iter[1,16]$broad_p_pick_up = p_picked_up
          pick_up_scenario_iter[1,17]$broad_opp_pick_up = opp_picked_up
          pick_up_scenario_iter[1,18]$Broad_Start_Order = Broad_Start_Order
          pick_up_scenario_iter[1,19]$Discard_Suit = discarded$suit
          pick_up_scenario_iter[1,20]$Discard_Value = discarded$value
          pick_up_scenarios = rbind(pick_up_scenarios, pick_up_scenario_iter)
          rm(pick_up_scenario_iter)
          rm(discarded)
        }
        pick_up_scenarios = pick_up_scenarios %>%
          mutate(Strongest_Card = case_when(Right_Bower > 0 ~ 13,
                                            Left_Bower > 0 ~ 12,
                                            TAce > 0 ~ 11,
                                            TKing > 0 ~ 10,
                                            TQueen > 0 ~ 9,
                                            TTen > 0 ~ 8,
                                            TNine > 0 ~ 7,
                                            Ace_count > 0 ~ 6,
                                            King_count > 0 ~ 5,
                                            Queen_count > 0 ~ 4,
                                            Jack_count > 0 ~ 3, 
                                            Ten_count > 0 ~ 2, 
                                            Nine_count > 0 ~ 1, TRUE ~ 0),
                 Card_Strength = case_when(Discard_Value == 'Nine' ~ 1,
                                           Discard_Value == 'Ten' ~ 2,
                                           Discard_Value == 'Jack' ~ 3,
                                           Discard_Value == 'Queen' ~ 4,
                                           Discard_Value == 'King' ~ 5,
                                           Discard_Value == 'Ace' ~ 6, TRUE ~ 0))
        
        pick_up_scenarios$pass_prob = round(predict(euchre_turn_up_model, newdata = pick_up_scenarios, type = 'probs')[,1], 2)
        pick_up_scenarios$order_trump_prob = round(predict(euchre_turn_up_model, newdata = pick_up_scenarios, type = 'probs')[,2], 2)
        pick_up_scenarios$order_trump_alone_prob = round(predict(euchre_turn_up_model, newdata = pick_up_scenarios, type = 'probs')[,3], 2)
        pick_up_scenarios$score = pick_up_scenarios$order_trump_prob + (pick_up_scenarios$order_trump_alone_prob * 1.5)
        pick_up_scenarios$model_prediction = predict(euchre_turn_up_model, newdata = pick_up_scenarios, type = 'class')
        pick_up_scenario = pick_up_scenarios[pick_up_scenarios$score == (max(pick_up_scenarios$score)), ]
        pick_up_scenario = pick_up_scenario[pick_up_scenario$Card_Strength == (min(pick_up_scenario$Card_Strength)), ][1,]
        discard_card = glue('{tolower(pick_up_scenario$Discard_Value)} of {tolower(pick_up_scenario$Discard_Suit)}')
        
      } else{
        # Simply use model to assess current hand if I'm not the dealer
        suit = turned_up$suit
        pick_up_scenario = data.frame()
        pick_up_scenario[1,1]$Right_Bower = nrow(cards[cards$value == 'Jack' & cards$suit == suit & cards[glue('{suit}_trump')] == 1,])
        pick_up_scenario[1,2]$Left_Bower = nrow(cards[cards$value == 'Jack' & cards$suit != suit & cards[glue('{suit}_trump')] == 1,])
        pick_up_scenario[1,3]$TAce = nrow(cards[cards$value == 'Ace' & cards$suit == suit, ])
        pick_up_scenario[1,4]$TKing = nrow(cards[cards$value == 'King' & cards$suit == suit, ])
        pick_up_scenario[1,5]$TQueen = nrow(cards[cards$value == 'Queen' & cards$suit == suit, ])
        pick_up_scenario[1,6]$TTen = nrow(cards[cards$value == 'Ten' & cards$suit == suit, ])
        pick_up_scenario[1,7]$TNine = nrow(cards[cards$value == 'Nine' & cards$suit == suit, ])
        pick_up_scenario[1,8]$Ace_count = nrow(cards[cards$value == 'Ace' & cards[glue('{suit}_trump')] != 1,])
        pick_up_scenario[1,9]$King_count = nrow(cards[cards$value == 'King' & cards[glue('{suit}_trump')] != 1,])
        pick_up_scenario[1,10]$Queen_count = nrow(cards[cards$value == 'Queen' & cards[glue('{suit}_trump')] != 1,])
        pick_up_scenario[1,11]$Jack_count = nrow(cards[cards$value == 'Jack' & cards[glue('{suit}_trump')] != 1,])
        pick_up_scenario[1,12]$Ten_count = nrow(cards[cards$value == 'Ten' & cards[glue('{suit}_trump')] != 1,])
        pick_up_scenario[1,13]$Nine_count = nrow(cards[cards$value == 'Nine' & cards[glue('{suit}_trump')] != 1,])
        pick_up_scenario[1,14]$Count_Trump = nrow(cards[cards[glue('{suit}_trump')] == 1,])
        pick_up_scenario[1,15]$Non_Trump_Suit_Count = length(unique(cards[cards[glue('{suit}_trump')] != 1,]$suit))
        pick_up_scenario[1,16]$broad_p_pick_up = p_picked_up
        pick_up_scenario[1,17]$broad_opp_pick_up = opp_picked_up
        pick_up_scenario[1,18]$Broad_Start_Order = Broad_Start_Order
        pick_up_scenario = pick_up_scenario %>%
          mutate(Strongest_Card = case_when(Right_Bower > 0 ~ 13,
                                            Left_Bower > 0 ~ 12,
                                            TAce > 0 ~ 11,
                                            TKing > 0 ~ 10,
                                            TQueen > 0 ~ 9,
                                            TTen > 0 ~ 8,
                                            TNine > 0 ~ 7,
                                            Ace_count > 0 ~ 6,
                                            King_count > 0 ~ 5,
                                            Queen_count > 0 ~ 4,
                                            Jack_count > 0 ~ 3, 
                                            Ten_count > 0 ~ 2, 
                                            Nine_count > 0 ~ 1, TRUE ~ 0))
        
        pick_up_scenario$pass_prob = round(predict(euchre_turn_up_model, newdata = pick_up_scenario, type = 'probs')[1], 2)
        pick_up_scenario$order_trump_prob = round(predict(euchre_turn_up_model, newdata = pick_up_scenario, type = 'probs')[2], 2)
        pick_up_scenario$order_trump_alone_prob = round(predict(euchre_turn_up_model, newdata = pick_up_scenario, type = 'probs')[3], 2)
        pick_up_scenario$score = pick_up_scenario$order_trump_prob + (pick_up_scenario$order_trump_alone_prob * 1.5)
        pick_up_scenario$model_prediction = predict(euchre_turn_up_model, newdata = pick_up_scenario, type = 'class')
      }
      
      # Assess second model that determines the best suit (excluding the turned up suit) that would be most ideal to bid 
      recommendation2 = NULL
      cutoff_1 = 0.1349509
      cutoff_2 = 3.314098
      
      suit_summaries = data.frame()
      for(suit in suits){
        if(suit == turned_up$suit){
          next
        }
        suit_summary = data.frame()
        suit_summary[1,1]$Suit = suit
        suit_summary[1,2]$Right_Bower = nrow(cards[cards$value == 'Jack' & cards$suit == suit & cards[glue('{suit}_trump')] == 1,])
        suit_summary[1,3]$Left_Bower = nrow(cards[cards$value == 'Jack' & cards$suit != suit & cards[glue('{suit}_trump')] == 1,])
        suit_summary[1,4]$TAce = nrow(cards[cards$value == 'Ace' & cards$suit == suit, ])
        suit_summary[1,5]$TKing = nrow(cards[cards$value == 'King' & cards$suit == suit, ])
        suit_summary[1,6]$TQueen = nrow(cards[cards$value == 'Queen' & cards$suit == suit, ])
        suit_summary[1,7]$TTen = nrow(cards[cards$value == 'Ten' & cards$suit == suit, ])
        suit_summary[1,8]$TNine = nrow(cards[cards$value == 'Nine' & cards$suit == suit, ])
        suit_summary[1,9]$Ace_count = nrow(cards[cards$value == 'Ace' & cards[glue('{suit}_trump')] != 1,])
        suit_summary[1,10]$King_count = nrow(cards[cards$value == 'King' & cards[glue('{suit}_trump')] != 1,])
        suit_summary[1,11]$Queen_count = nrow(cards[cards$value == 'Queen' & cards[glue('{suit}_trump')] != 1,])
        suit_summary[1,12]$Jack_count = nrow(cards[cards$value == 'Jack' & cards[glue('{suit}_trump')] != 1,])
        suit_summary[1,13]$Ten_count = nrow(cards[cards$value == 'Ten' & cards[glue('{suit}_trump')] != 1,])
        suit_summary[1,14]$Nine_count = nrow(cards[cards$value == 'Nine' & cards[glue('{suit}_trump')] != 1,])
        suit_summary[1,15]$Count_Trump = nrow(cards[cards[glue('{suit}_trump')] == 1,])
        suit_summary[1,16]$Non_Trump_Suit_Count = length(unique(cards[cards[glue('{suit}_trump')] != 1,]$suit))
        suit_summary[1,17]$Broad_Start_Order = Broad_Start_Order
        suit_summaries = rbind(suit_summaries, suit_summary)
      }
      suit_summaries = suit_summaries %>%
        mutate(Strongest_Card = case_when(Right_Bower > 0 ~ 13,
                                          Left_Bower > 0 ~ 12,
                                          TAce > 0 ~ 11,
                                          TKing > 0 ~ 10,
                                          TQueen > 0 ~ 9,
                                          TTen > 0 ~ 8,
                                          TNine > 0 ~ 7,
                                          Ace_count > 0 ~ 6,
                                          King_count > 0 ~ 5,
                                          Queen_count > 0 ~ 4,
                                          Jack_count > 0 ~ 3, 
                                          Ten_count > 0 ~ 2, 
                                          Nine_count > 0 ~ 1, TRUE ~ 0),
               Non_Trump_Turned_Down_Color_Ind = case_when(turned_up$suit == 'Hearts' & Suit == 'Diamonds' ~1,
                                                           turned_up$suit == 'Diamonds' & Suit == 'Hearts' ~1,
                                                           turned_up$suit == 'Clubs' & Suit == 'Spades' ~1,
                                                           turned_up$suit == 'Spades' & Suit == 'Clubs' ~1, TRUE ~0))
      suit_summaries$pass_prob = suppressWarnings(round(predict(euchre_turn_down_model, newdata = suit_summaries, type = 'fitted.ind')[,1], 2))
      suit_summaries$order_trump_prob = suppressWarnings(round(predict(euchre_turn_down_model, newdata = suit_summaries, type = 'fitted.ind')[,2], 2))
      suit_summaries$order_trump_alone_prob = suppressWarnings(round(predict(euchre_turn_down_model, newdata = suit_summaries, type = 'fitted.ind')[,3], 2))
      suit_summaries$score = suit_summaries$order_trump_prob + (suit_summaries$order_trump_alone_prob * 1.5)
      suit_summaries$model_prediction = suppressWarnings(predict(euchre_turn_down_model, newdata = suit_summaries, type = 'lp'))
      suit_summaries = suit_summaries %>%
        mutate(model_prediction = case_when(model_prediction<=cutoff_1~0, model_prediction<=cutoff_2~1, TRUE~2))
      best_suits = suit_summaries[suit_summaries$score == (max(suit_summaries$score)), ]
      best_suit = best_suits[1,]
      
      # Determine best suit(s)
      if(nrow(best_suits) > 1){
        suit1 = best_suits[1,]$Suit
        suit2 = best_suits[2,]$Suit
        the_best_suit = glue('either {tolower(suit1)} or {tolower(suit2)}')
      } else{
        the_best_suit = tolower(best_suits$Suit)
      }
      
      # Create recommendation to be followed, based on model outputs
      if(dealer == 'Me'){
        if(pick_up_scenario$model_prediction == 0){
          recommendation1 = glue("Pass on {tolower(turned_up$suit)}.")
          note = glue("Discard the {discard_card} if you're ordered to pick it up.")
        } else if(pick_up_scenario$model_prediction == 1){
          recommendation1 = glue("Pick it up. Discard the {discard_card} from your hand.")
        } else if(pick_up_scenario$model_prediction == 2){
          recommendation1 = glue("Pick it up and go alone. Discard the {discard_card} from your hand.")
        }
        
        if(best_suit$model_prediction == 0){
          recommendation2 = glue("Pass on the other suits.")
        } else if(best_suit$model_prediction == 1){
          recommendation2 = glue("If you have the opportunity, bid {the_best_suit}.")
        } else if(best_suit$model_prediction == 2){
          recommendation2 = glue("If you have the opportunity, bid {the_best_suit} and go alone.")
        }
        
        if(pick_up_scenario$model_prediction == 0){
          if(best_suit$model_prediction == 0){
            recommendation = glue("Pass on everything.")
            note = glue("If you're ordered to pick it up, discard the {discard_card}. If you're required to make a bid on one of the three other suits, bid {the_best_suit}.")
          } else{
            recommendation = glue("{recommendation1} {recommendation2}")
          }
        } else{
          recommendation = recommendation1
          }
        
      } else{
        if(pick_up_scenario$model_prediction == 0){
          recommendation1 = glue("Pass on {tolower(turned_up$suit)}.")
        } else if(pick_up_scenario$model_prediction == 1){
          recommendation1 = "Order to pick it up."
        } else if(pick_up_scenario$model_prediction == 2){
          recommendation1 = "Order to pick it up and go alone."
        }
        
        if(best_suit$model_prediction == 0){
          recommendation2 = "Pass on the other suits."
        } else if(best_suit$model_prediction == 1){
          recommendation2 = glue("If you have the opportunity, bid {the_best_suit}.")
        } else if(best_suit$model_prediction == 2){
          recommendation2 = glue("If you have the opportunity, bid {the_best_suit} and go alone.")
        }
        
        if(Broad_Start_Order == 'First'){
          if(pick_up_scenario$model_prediction == 0){
            if(best_suit$model_prediction == 0){
              recommendation = 'Pass on everything.'
            } else{
              recommendation = glue('{recommendation1} {recommendation2}')
            }
          } else if((pick_up_scenario$model_prediction == 1 | pick_up_scenario$model_prediction == 2) & pick_up_scenario$score < best_suit$score){
            recommendation = glue('Pass on {tolower(turned_up$suit)}. {recommendation2}')
            note = glue('You have a good hand for {tolower(turned_up$suit)}, but a better hand for {the_best_suit}. Take advantage of bidding first to get the best situation for yourself.')
            } else{
              recommendation = recommendation1
              }
          } else{
            if(pick_up_scenario$model_prediction == 0){
              if(best_suit$model_prediction == 0){
                recommendation = "Pass on everything."
                } else{
                  recommendation = glue("{recommendation1} {recommendation2}")
                  }
              } else{
                recommendation = recommendation1
              }
          }
      }
      # Add disclaimer explanation after recommendation
      explanation = "The model recommendation won't always be correct. The table below shows the probabilities that each decision is correct. The model recommendation does not always correspond with the highest probability value shown. Depending on your game situation, you may want to utilize the probabilities to either be more conservative or more aggressive when making a decision."
      
      # Create probability table that shows the probability of a hand falling into each specific class. Row visibility is determined based on model output 
      if(dealer == 'Me'){
        bid_probability_table = cbind(action = glue('Picking up the {tolower(turned_up$value)} of {tolower(turned_up$suit)}'), select(pick_up_scenario, pass_prob, order_trump_prob, order_trump_alone_prob))
      } else{
        bid_probability_table = cbind(action = glue('Ordering up the {tolower(turned_up$value)} of {tolower(turned_up$suit)}'), select(pick_up_scenario, pass_prob, order_trump_prob, order_trump_alone_prob))
      }
      
      if(Broad_Start_Order == 'First' & (pick_up_scenario$model_prediction == 1 | pick_up_scenario$model_prediction == 2) & pick_up_scenario$score >= best_suit$score){
        bid_probability_table = bid_probability_table
      } else if(((pick_up_scenario$model_prediction == 1 & (pick_up_scenario$order_trump_prob - pick_up_scenario$pass_prob) <= .25)) | pick_up_scenario$model_prediction == 0 | (Broad_Start_Order == 'First' & pick_up_scenario$score < best_suit$score)){
        bid_probability_table_ext = cbind(action = glue('Bidding {the_best_suit}'), select(best_suit, pass_prob, order_trump_prob, order_trump_alone_prob))
        bid_probability_table = rbind(bid_probability_table, bid_probability_table_ext)
      } 
      
      bid_probability_table = bid_probability_table %>%
        rename('p_pass_is_correct' = 'pass_prob', 'p_bid_suit_is_correct' = 'order_trump_prob', 'p_bid_suit_go_alone_is_correct' = 'order_trump_alone_prob')
      
      rownames(bid_probability_table) = bid_probability_table$action
      bid_probability_table$action = NULL
      
      # Add reset button that appears when a recommendation is made to easily add new records 
      output$Reset_Button = renderUI({
        actionButton(inputId = "Reset_Button", label = "Reset", style = "font-size: 20px; padding: 10px 20px; width: 200px; height: 50px; background-color: #f0f0f0")
      })
    }
    
    # Render outputs 
    
    output$RecText = renderText({
      recommendation
    })
    
    output$AdlRecText = renderText({
      note
    })
    
    output$myExplanation = renderText({
      explanation
    })
    
    output$RecTable = renderDT({
      datatable(bid_probability_table, options = list(dom = "t"))
      })
    
  })
  
  # Add reset button actions that clear outputs and change entries to default values
  observeEvent(input$Reset_Button, {
    updateSelectInput(session = getDefaultReactiveDomain(), "Card1value", selected = " ")
    updateSelectInput(session = getDefaultReactiveDomain(), "Card2value", selected = " ")
    updateSelectInput(session = getDefaultReactiveDomain(), "Card3value", selected = " ")
    updateSelectInput(session = getDefaultReactiveDomain(), "Card4value", selected = " ")
    updateSelectInput(session = getDefaultReactiveDomain(), "Card5value", selected = " ")
    updateSelectInput(session = getDefaultReactiveDomain(), "Card1suit", selected = " ")
    updateSelectInput(session = getDefaultReactiveDomain(), "Card2suit", selected = " ")
    updateSelectInput(session = getDefaultReactiveDomain(), "Card3suit", selected = " ")
    updateSelectInput(session = getDefaultReactiveDomain(), "Card4suit", selected = " ")
    updateSelectInput(session = getDefaultReactiveDomain(), "Card5suit", selected = " ")
    updateSelectInput(session = getDefaultReactiveDomain(), "TurnedUpvalue", selected = " ")
    updateSelectInput(session = getDefaultReactiveDomain(), "TurnedUpsuit", selected = " ")
    updateSelectInput(session = getDefaultReactiveDomain(), "Dealer", selected = " ")

    output$RecText = renderText({
      NULL
    })
    
    output$myExplanation = renderText({
      NULL
    })
    
    output$RecTable = renderDT({
      NULL
    })
    
    output$AdlRecText = renderText({
      NULL
    })
    
    output$Reset_Button = renderUI({})
  })
}

# Run app
shinyApp(ui = ui, server = server)




