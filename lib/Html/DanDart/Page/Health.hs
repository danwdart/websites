{-# LANGUAGE OverloadedStrings #-}

module Html.DanDart.Page.Health where

import Control.Monad.Reader
import Data.Env.Types
import Data.NonEmpty               qualified as NE
import Html.Common.Link
import Html.Common.Page
import Html.Common.Shortcuts
import Text.Blaze.Html5      as H hiding (main)

pageHealth ∷ (MonadReader Website m) ⇒ m Html
pageHealth = plainBreadcrumb (NE.trustedNonEmpty "Health") . makePage "health" "Health" defaultLayout notDefaultPage $ do
    p "Both my physical and mental health are very low at the moment, but I am always more than happy to talk about them."
    p "I think I'm addicted to caffeine, which I wouldn't recommend."
    p "I have been diagnosed with the following things, both physical and mental intermingling:"
    ul $ do

        li $ extLinkTitle (nhs <> "post-traumatic-stress-disorder-ptsd") "Post-traumatic Stress Disorder" "PTSD"
        li $ extLink (nhs <> "fibromyalgia") "Fibromyalgia"
        li $ extLink (nhs <> "autism") "Asperger's Syndrome"
        li $ do
            extLink (nhs <> "attention-deficit-hyperactivity-disorder-adhd") "ADHD"
            ", including "
            extLink (nhs <> "memory-loss-amnesia/") "short-term memory loss"
        li $ extLink (nhs <> "generalised-anxiety-disorder") "Anxiety"
        li $ extLink (nhs <> "depression") "Depression"
        li $ extLink (nhs <> "sleep-apnoea") "Sleep apnoea"
