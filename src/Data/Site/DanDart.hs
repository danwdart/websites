{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Data.Site.DanDart (keywords, descTitle, musicalStyles, favCharacters, musicList) where

import           Html.Common.Shortcuts
import           Text.Blaze.Html5      as H hiding (main)

keywords ∷ [AttributeValue]
keywords = [
    "dan",
    "dart",
    "software",
    "engineer",
    "mathematics",
    "lover",
    "radio",
    "ham",
    "php",
    "javascript",
    "css",
    "coffee",
    "coffeescript",
    "laravel",
    "zend",
    "framework",
    "linux",
    "gnu",
    "express.js",
    "ubuntu",
    "debian"
    ]

descTitle ∷ String
descTitle = "Dan Dart: Software Engineer, Mathematics Lover, Radio Ham, Musician"

musicalStyles ∷ [Html]
musicalStyles = [
    "Prog Rock",
    "Heavy Metal",
    "Ambient",
    "Classical"
    ]

favCharacters ∷ [(Html, AttributeValue, [(Html, AttributeValue, Html)])]
favCharacters = [
    (
        "Star Trek",
        wikipedia "Star_Trek",
        [
            (
                "Spock",
                wikia "memory-alpha" "Spock",
                "I often think of the world logically"
            ),
            (
                "Evil (Mirror) Spock",
                wikia "memory-alpha" "Spock_(mirror)",
                "of his outrageous beard and he's just hysterical"
            ),
            (
                "Data",
                wikia "memory-alpha" "Data",
                "I am curious, confused and if he was human he'd probably be autistic"
            ),
            (
                "EMH",
                wikia "memory-alpha" "Emergency_Medical_Holographic_program",
                "he's socially inept, evolves and gets into hilariously unfortunate situations a lot. Also, " <> babby "Seven" <> " is waifu."
            ),
            (
                "Odo",
                wikia "memory-alpha" "Odo",
                "he's an outsider, is awesome and is sick."
            )
        ]
    ),
    (
        "Kingdom Hearts",
        wikipedia "Kingdom_Hearts",
        [
            (
                "Sora",
                fandom "kingdomhearts" "Sora",
                babby "Kairi" <> " is my waifu and princess and I'm kinda new to this."
            )
        ]
    ),
    (
        "Darling in the FranXX",
        wikipedia "Darling_in_the_Franxx",
        [
            (
                "Hiro (016)",
                fandom "darling-in-the-franxx" "Hiro",
                babby "Zero Two" <> " is waifu and I am oblivious."
            )
        ]
    ),
    (
        "Danganronpa",
        wikipedia "Danganronpa",
        [
            (
                "Chihiro Fujisaki",
                fandom "danganronpa" "Chihiro_Fujisaki",
                "I love programming and feel similar about life. " <> babby "Touko" <> " is waifu."
            ),
            (
                "Nagito Komaeda",
                fandom "danganronpa" "Nagito_Komaeda",
                "I'm very lucky and obsessed with hope, and also trash. " <> babby "Tsumiki" <> " is waifu."
            ),
            (
                "Shuichi Saihara",
                fandom "danganronpa" "Shuichi_Saihara",
                "he's cute, a little bit emo and evolves like I do. " <> babby "Himiko" <> " is waifu."
            )
        ]
    ),
    (
        "Aku no Hana",
        wikipedia "The_Flowers_of_Evil_(manga)",
        [
            (
                "Takao Kasuga",
                "https://myanimelist.net/character/62795/Takao_Kasuga",
                "I live in a boring town and have been improved by my darling " <> babby "Nakamura"
            )
        ]
    ),
    (
        "Soul Eater",
        wikipedia "Soul_Eater_(manga)",
        [
            (
                "Dr. Franken Stein",
                wikia "souleater" "Franken_Stein",
                "he is nutty and likes to spin in chairs. My waifu is " <> babby "Crona" <> "."
            )
        ]
    ),
    (
        "Koe no Katachi",
        wikipedia "A_Silent_Voice_(film)",
        [
            (
                "Shōya Ishida",
                wikia "koenokatachi" "Shōya_Ishida",
                "he was transformed into a kind boy, and " <> babby "Shoko" <> " is waifu..."
            )
        ]
    ),
    (
        "Kimi no Na wa",
        wikipedia "Your_Name",
        [
            (
                "Taki Tachibana",
                wikia "kiminonawa" "Taki_Tachibana",
                "I just really relate to the lost / saving story."
            )
        ]
    ),
    (
        "Boku dake ga Inai Machi",
        wikipedia "Erased_(manga)#Anime",
        [
            (
                "Satoru Fujinuma",
                wikia "bokudakegainaimachi" "Satoru_Fujinuma",
                "I love time travel and would totally do this."
            )
        ]
    ),
    (
        "Tenki no Ko",
        wikipedia "Weathering_with_You",
        [
            (
                "Hodaka Morishima",
                wikia "weatheringwithyou" "Hodaka_Morishima",
                "my waifu is a weather goddess."
            )
        ]
    ),
    (
        "Yuri on Ice",
        wikipedia "Yuri_on_Ice",
        [
            (
                "Yuuri Katsuki",
                wikia "yurionice" "Yuuri_Katsuki",
                "that story was so cute and I see myself as a noob that grows."
            )
        ]
    ),
    (
        "Steven Universe",
        wikipedia "Steven_Universe",
        [
            (
                "Amethyst",
                wikia "steven-universe" "Amethyst",
                "she's got \"a system\" and I like her vibe."
            ),
            (
                "(Um)Greg",
                wikia "steven-universe" "Greg_Universe",
                "of his guitar skills."
            ),
            (
                "Steven Universe (from the movie)",
                wikia "steven-universe" "Steven_Universe_(character)",
                "his evolution to this stage helps my waifu " <> babby "Spinel" <> "."
            )
        ]
    ),
    (
        "Kobayashi-san Chi no Maid Dragon",
        wikipedia "Miss_Kobayashi%27s_Dragon_Maid",
        [
            (
                "Fafnir",
                wikia "maid-dragon" "Fafnir",
                "he's dapper, awesome, interesting and acts hilariously. My waifu is " <> babby "Kanna" <> "."
            )
        ]
    ),
    (
        "Mirai Nikki",
        wikipedia "Future_Diary",
        [
            (
                "Yukiteru Amano",
                wikia "futurediary" "Yukiteru_Amano",
                "my waifu is " <> babby "Yuno" <> ", Yukki is depressed and I would act like he acts in the OVA"
            )
        ]
    ),
    (
        "Berserk",
        wikipedia "Berserk_%28manga%29",
        [
            (
                "Griffith",
                wikipedia "List_of_Berserk_characters#Griffith",
                "he's sneaky and smart and he looks fabulous. " <> babby "Casca" <> " is waifu."
            )
        ]
    ),
    (
        "Kuroshitsuji",
        wikipedia "Black_Butler",
        [
            (
                "Grell Sutcliff",
                fandom "kuroshitsuji" "Grell_Sutcliff",
                "he's hysterical, fabulous and has good taste in butlers"
            )
        ]
    ),
    (
        "Mahoutsukai no Yome",
        wikipedia "The_Ancient_Magus%27_Bride",
        [
            (
                "Elias Ainsworth",
                wikia "ancientmagusbride" "Elias_Ainsworth",
                "he's caring, doesn't act like outrageous humans and " <> babby "Chise" <> " is my waifu"
            )
        ]
    ),
    (
        "Neon Genesis Evangelion",
        wikipedia "Neon_Genesis_Evangelion",
        [
            (
                "Shinji Ikari",
                wikipedia "Shinji_Ikari",
                "existentialism, essentially. He's a whiner, but I like his thought patterns when he's being existential. " <> babby "Rei" <> " is waifu."
            )
        ]
    ),
    (
        "MLP",
        wikipedia "My_Little_Pony:_Friendship_Is_Magic",
        [
            (
                "Twilight Sparkle",
                wikipedia "List_of_My_Little_Pony%3A_Friendship_Is_Magic_characters#Twilight_Sparkle",
                "knowledge and being awesome. " <> babby "Fluttershy" <> " is waifu."
            )
        ]
    ),
    (
        "Alice's Adventures in Wonderland",
        wikipedia "Alice%27s_Adventures_in_Wonderland",
        [
            (
                "The Mad Hatter",
                wikipedia "Hatter_(Alice%27s_Adventures_in_Wonderland)",
                "I like his sense of humour, eccentricity and hats"
            )
        ]
    ),
    (
        "The Filthy Frank Show",
        ytUser <> "TVFilthyFrank",
        [
            (
                "Real Frank",
                wikia "filthy-frank" "Filthy_Frank",
                "he's lost and good to his crew. ^Also depression, yay^"
            )
        ]
    ),
    (
        "Back to the Future",
        wikipedia "Back_to_the_Future_(franchise)",
        [
            (
                "\"Doc\" Emmett Brown",
                wikipedia "Emmett_Brown",
                "he's eccentric and time travel, hell yeah"
            )
        ]
    ),
    (
        "Doctor Who",
        wikipedia "Doctor_Who",
        [
            (
                "The Doctor",
                wikipedia "The_Doctor_(Doctor_Who)",
                "of eccentricity, inventions and awesomeness. My favourite Doctor is the fourth, played by Tom Baker."
            )
        ]
    ),
    (
        "Battleborn",
        wikipedia "Battleborn_(video_game)",
        [
            (
                "Marquis",
                fandom "battleborn" "Marquis",
                "those quotes are amazing, dunk dunk dunk! My waifu is " <> babby "Orendi" <> "."
            )
        ]
    )
    ]



musicList ∷ [(Html, [Html])]
musicList = [
    (
        "Pink Floyd",
        [
            "Comfortably Numb",
            "Another Brick in the Wall",
            "Wish You Were Here",
            "The Dark Side of the Moon (yes, to me it's a single song)"
        ]
    ),
    (
        "Focus",
        [
            "Anonymus II",
            "Eruption",
            "Sylvia/Hocus Pocus"
        ]
    ),
    (
        "16volt",
        [
            "At The End",
            "Therapy"
        ]
    ),
    (
        "Bring Me The Horizon",
        ["That's The Spirit (the whole album is amazing!)"]
    ),
    (
        "Starset",
        ["My Demons"]
    ),
    (
        "Bach",
        ["Wohltemperierte Klavier"]
    ),
    (
        "Holst",
        ["Planets"]
    ),
    (
        "Many more...",
        []
    )
    ]
