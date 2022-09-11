{-# LANGUAGE OverloadedStrings #-}

module Data.Site.DanDart where

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

pokémonList ∷ [String]
pokémonList = [
    "Eevee",
    "Vulpix",
    "Rowlet",
    "Piplup"
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
                "Q",
                wikia "memory-alpha" "Q",
                "I love what a cheeky devil he is, and his ability to make people think different of themselves is fascinating"
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
                "it's such a sweet story of saving each others' lives constantly, and I can identify heavily with that. My " <> babby "Zero Two" <> " and I love each other just as dearly and share many great moments, expressions and feelings from the series."
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
        "Hyouka",
        wikipedia "Hyouka",
        [
            (
                "Houtarou Oreki",
                fandom "hyouka" "Houtarou_Oreki",
                "his energy-efficient philosophy is something I can heavily agree with. Plus, I think he must have similar mental illnesses to me and/or be a spoonie like me. My waifu is " <> babby "Eru Chitanda" <> "."
            )
        ]
    ),
    (
        "Toradora!",
        wikipedia "Toradora!",
        [
            (
                "Ryuuji Takasu",
                fandom "tora-dora" "Ryuuji_Takasu",
                "he is very applicable to my life, and he loves very hard, saving and helping my darling " <> babby "Taiga" <> ", and their love music makes me cry every time."
            )
        ]
    ),
    (
        "Ijiranaide, Nagatoro-san",
        wikipedia "Don't_Toy_with_Me,_Miss_Nagatoro",
        [
            (
                "Naoto Hachiouji",
                fandom "nagatoro" "Naoto_Hachiouji",
                "he is very awkward like me, and tolerates and eventually enjoys the loving tsumdere/yandere pestering and loving harrassment of his " <> babby "Nagatoro" <> ". We match very well."
            )
        ]
    ),
    (
        "Tonikaku Kawaii (Tonikawa)",
        wikipedia "Fly_Me_to_the_Moon_(manga)#Anime",
        [
            (
                "Nasa Yuzaki",
                fandom "tonikaku-kawaii" "Nasa_Yuzaki",
                "he falls very hard in love enough to marry his loving wife " <> babby "Tsukasa" <> " early on, but is incredibly awkward. Also, space."
            )
        ]
    ),
    (
        "Blend S",
        wikipedia "Blend_S#Anime",
        [
            (
                "Dino",
                fandom "blend-s" "Dino",
                "he stays up late to watch anime and adores his crush " <> babby "Maika" <> ". Also, he's foreign, and I feel that way very often."
            )
        ]
    ),
    (
        "Hataraku Maō-sama! (The Devil Is a Part-Timer!)",
        wikipedia "The_Devil_Is_a_Part-Timer!#Anime",
        [
            (
                "Sadao Maou / King Satan",
                fandom "hataraku" "Sadao_Maou",
                "he is the only one in the main demon cast who works, and his dedication to his work is admirable. If I was anyone, I'm him. My waifu is " <> babby "Chiho" <> "."
            )
        ]
    ),
    (
        "Fate/stay night: Heaven's Feel",
        wikipedia "Fate%2Fstay_night%3A_Heaven's_Feel",
        [
            (
                "Shirou Emiya",
                fandom "typemoon" "Shirou_Emiya",
                "he protects the abused baby and helps her, as I do with my waifu " <> babby "Sakura"
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
        "Hazbin Hotel",
        wikipedia "Hazbin_Hotel",
        [
            (
                "Alastor",
                fandom "hazbinhotel" "Alastor",
                "I love radio as a technology, am a ham radio enthusiast and think he's hilarious. My waifu is " <> babby "Vaggie"
            )
        ]
    ),
    (
        "Helluva Boss",
        wikipedia "Helluva_Boss",
        [
            (
                "Moxxie",
                fandom "helluvaboss" "Moxxie",
                "his relationship reflects the kind of sappy, lovesick relationship that I have with my " <> babby "Millie"
            ),
            (
                "Stolas",
                fandom "helluvaboss" "Stolas",
                "he is hilarious and his relationship with his daughter reflects the kind of relationship I have with my " <> babby "Octavia"
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
                "Peridot",
                wikia "steven-universe" "Peridot",
                "of her mannerisms, special interests, need for technology, weirdness, evil laughter and general vibe. She probably has the gem equivalent of autism. My waifu is " <> babby "Lapis" <> "."
            ),
            (
                "Amethyst",
                wikia "steven-universe" "Amethyst",
                "she's got \"a system\" and I like her vibe."
            ),
            (
                "(Um)Greg",
                wikia "steven-universe" "Greg_Universe",
                "of his guitar skills and unconditional love of the gang. Also I feel like I'm dumb a lot and don't want to get involved in business that I might mess up."
            ),
            (
                "Steven Universe (from the movie)",
                wikia "steven-universe" "Steven_Universe_(character)",
                "his evolution to this stage helps my waifu " <> babby "Spinel" <> "."
            )
        ]
    ),
    (
        "Undertale",
        wikipedia "Undertale",
        [
            (
                "W. D. Gaster",
                wikia "undertale" "W._D._Gaster",
                "he is mysterious and talks in code, much like I often do because of my autism."
            ),
            (
                "Alphys",
                wikia "undertale" "Alphys",
                "she's a super nerd and grows to not be afraid to express her emotions."
            )
        ]
    ),
    (
        "Dead or Alive 6",
        wikipedia "Dead_or_Alive_6",
        [
            (
                "NiCO",
                wikia "deadoralive" "Nico",
                "of her obsession with many forms of science and interest with experimentation."
            )
        ]
    ),
    (
        "Drakengard 3",
        wikipedia "Drakengard_3",
        [
            (
                "Accord",
                wikia "drakengard" "Accord",
                "of her shitbag-like attitude, understanding and screwing around of alternate timelines and interest in psychology."
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
                "Griffith (pre-Femto)",
                wikipedia "List_of_Berserk_characters#Griffith",
                "he's sneaky and smart and he looks fabulous. " <> babby "Schierke" <> " is waifu."
            )
        ]
    ),
    (
        "Kuroshitsuji",
        wikipedia "Black_Butler",
        [
            (
                "Grell Sutcliff [Season 1-2]",
                fandom "kuroshitsuji" "Grelle_Sutcliff",
                "he's hysterical, fabulous and has good taste in butlers (please note I do not know anything about this person after season 2)"
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
        "Ace Attorney",
        wikipedia "Ace_Attorney",
        [
            (
                "Phoenix Wright",
                wikipedia "Phoenix_Wright",
                "he's hilariously unaware, tries to be good at what he does but fails often" -- babby
            )
        ]
    ),
    (
        "Strawberry Panic!",
        wikipedia "Strawberry_Panic!",
        [
            (
                "Shizuma Hanazono",
                fandom "strawberrypanic" "Shizuma_Hanazono",
                "she yans over my waifu, who is both " <> babby "Nagisa and Kaori"
            ),
            (
                "Tamao Suzumi",
                fandom "strawberrypanic" "Tamao_Suzumi",
                "she fights over my waifu, who is " <> babby "Nagisa"
            )
        ]
    ),
    (
        "Kannazuki no Miko",
        wikipedia "Kannazuki_no_Miko",
        [
            (
                "Chikane Himemiya",
                fandom "kannazukinomiko" "Chikane_Himemiya",
                "she adores and fights for my waifu, " <> babby "Himeko"
            )
        ]
    ),
    (
        "Kyōshirō to Towa no Sora",
        wikipedia "Shattered_Angels",
        [
            (
                "Kaon",
                fandom "shatteredangels" "kaon",
                "she adores my waifu " <> babby "Himiko" <> " and is Chikane's reincarnation"
            )
        ]
    ),
    (
        "Mahō Shōjo Madoka Magika",
        wikipedia "Puella_Magi_Madoka_Magica",
        [
            (
                "Homura Akemi",
                fandom "madoka" "Homura_Akemi",
                "she goes after my waifu " <> babby "Madoka" <> "'s heart infinite times in infinite universes"
            )
        ]
    ),
    (
        "Yami to Bōshi to Hon no Tabibito",
        wikipedia "Yami_to_Bōshi_to_Hon_no_Tabibito",
        [
            (
                "Hazuki Azuma",
                fandom "yuripedia" "Hazuki_Azuma",
                "she is in love with my waifu " <> babby "Hatsumi"
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
