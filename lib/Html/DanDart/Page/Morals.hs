{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.DanDart.Page.Morals where

import           Data.Env.Types
import           Html.Common.Link
import           Html.Common.Page
import           Text.Blaze.Html5            as H hiding (main)
import           Text.Blaze.Html5.Attributes as A

pageMorals ∷ WebsiteM Html
pageMorals = makePage "morals" "Morals" defaultLayout notDefaultPage $ do
    p $ do
        "My morals are as follows. I plan to be active about the following issues:"
        ol $ do
            li . strong $ "Generally"
            ol $ do
                li "Help as many people as possible"
                li "Hinder or hurt as few people as possible"
                li "Everybody regardless of gender, race, orientation or any differentiation to anybody else has the same rights."
            li "Those who cannot help themselves should be helped in the appropriate way:"
            ol $ do
                li . strong $ "The disabled"
                ol $ do
                    li "Physical restrictions on movement that currently exist shall be minimised to the limit of human capacity. This means appropriate disabled access is required everywhere."
                    li "Since disability is not usually the person's fault, "
                li . strong $ "The homeless"
                ol $ do
                    li "The homeless do not deserve to be homeless, so it is a branch of the government's duty to house them."
                    li "Public property should be a reasonable place to sleep barring using the provided facilities providing it disturbs no one."
                    li "The homeless must be fed, sheltered and clothed as much as anybody else."
                    li "It is nobody's right to cause somebody to be homeless."
                li . strong $ "The otherwise disadvantaged:"
                ol $ do
                    li "Asylum seekers must be aided."
            li $ do
                strong "Happiness"
                " is a basic right:"
                ol $ do
                    li "No one has the right to bully anybody else for any reason. This includes hate speech, and penalties for this must be higher than they were in 2022."
            li $ do
                strong "Health"
                " is a basic right:"
                ol $ do
                    li "It is important to keep to good standards on healthcare."
                    li "Everyone must be able to be helped with their health via a pool and not pay too much for any one treatment."
                    li "Everyone must be able to maintain a good standard for what is possible for their own health without having to worry about external factors like price."
                    li "Certain acts of changing one's body irreversibly must only be able to be done when the owner of the body can consent to invasive and permanent surgeries, and therefore should be restricted to the standard age of consent."
                    li "The terminally ill have the right to assisted suicide, as long as a psychological evaluation proves their ability to decide."
            li $ do
                strong "Education"
                " is a basic right:"
                ol $ do
                    li "The state has the fundamental duty to ensure everybody is educated to a basic level and has the appropriate opportunities to be able to be educated as far as they wish to be without issue. This means:"
                    ol $ do
                        li "Cheap (as far as the person is able to afford) education, and free if the person is on benefits, for such reasons as being without a job or being disabled"
            li $ do
                strong "Food"
                " is a basic right:"
                ol $ do
                    li "Everyone has the right to eat, regardless of whether they have a job. This means benefits for those not working or not able to work must include provisions for food and preparation."
                    li "Everyone must have the infrastructure and facilities to make and prepare food."
            li $ do
                strong "Loving"
                " is a basic right:"
                ol $ do
                    li "Being able to love, physically or emotionally, another consenting adult or group of adults, nothing must stand in the way."
                    li "Expressing this love by marriage or another means is the choice of the consenting adults and there must be nothing in the way of this also."
                    li "Everyone must be able to adopt pending reasonable background checks."
                    li "The age of consent for sexual acts must not differ between homosexual and heterosexual acts, but must be maintained at the boundary of adulthood, 18."
            li $ do
                strong "Safety"
                " is a basic right:"
                ol $ do
                    li "Everyone has the right to live safely and without fear. Anything that prevents this must be prevented itself."
            li $ do
                strong "Affordability"
                " is a basic right:"
                ol $ do
                    li "Basic wage is a must for any state, and this is equal or greater to the agreed upon living wage."
            li $ do
                strong "Abortion"
                " is a basic right:"
                ol $ do
                    li "Whether it is for health or any other reason, it is always up to the bearer of the child whether or not to abort."
                    li "Institutions must provide adequate amounts of birth control, sex education and social services to reduce the amount of abortions that even needed to happen."
            li $ do
                strong "Freedom of religion"
                " is a basic right, up to the following restrictions:"
                ol $ do
                    li "One must be free to exercise one's own personal worship practices without interfering in others' personal worship, rights or choices."
                    li "Religion is personal. Everyone must be free to decide one's own religion (or not), and one religion must not be shown and recommended to anybody over another."
                    li "Nothing must be charged for religious practices. Any religious institutions which takes profits must be taxed as if a business. Only those which do not can be treated as a charity."
            li $ do
                strong "Freedom of speech"
                " is a basic right, up to the following restrictions:"
                ol $ do
                    li "This does not include hate speech."
                    li "Flag burning is also counted as free speech, as an expression of dislike of a group. Group criticism should be dealt with in a suggestion of improvement instead of backlash."
            li $ do
                strong "Keeping the environment clean"
                " is extremely important:"
                ol $ do
                    li "Everyone has the duty to recycle where they can."
                    li "Measures must always be taken to reduce the amount of litter produced. This includes a ban on single-use plastics and such things, until and unless they can be broken down, such as by newly-discovered bacteria."
                    li "A limit is to be put on the amount of litter put in landfills per year, and each year this must decrease."
            -- li "Animals have the same rights:"
            li $ do
                strong "Drugs"
                " are often necessary:"
                ol $ do
                    li "All addictive substances must be decriminalised, and measures must be taken to help those addicted to some dangerous substances."
                    li "Especially harmfolsubstances must not be sold without a prescription or licence. This includes tobacco, nicotine based substances and large or persistent amounts of alcohol."
            li $ do
                strong "Dangerous items such as arms"
                " must be deconstructed and disposed of safely. Until this can happen:"
                ol $ do
                    li "Strong arms control for all citizens, requiring licencing, mental health background checks, etc."
                    li "No money is to be spent on any kind of weaponry and any left must be disabled, disposed of safely and sold for parts."
            li $ do
                strong "Warring"
                " is the solution to nothing and there is no excuse for it, therefore:"
                ol $ do
                    li "Nuclear devices are to be immediately defunded and funds diverted into more noble causes."
                    li "International neutrality must be upheld, which means leaving and dissolving all war and arms alliances, including the likes of NATO."
                    li "Combat roles must be limited, dissuaded and finally dissolved, and until they can be, every consenting adult must be able to have the same rights in terms of application - that is, passing the same physical tests."
            li $ do
                strong "Companies and trading"
                " must be heavily regulated."
                ol $ do
                    li "Companies must be shown to have a sufficiently diverse and inclusive outward presence."
                    li "Companies must treat everyone with the appropriate skill level equally when hiring."
                    li "Companies must pay a rate agreed to be equal to the skill level of the staff member regardless of other factors."
                    li "Trading must be no more restricted across borders when a geographic entity leaves a union."
                    li "There must be no such thing as a zero-hour contract as this constitutes abuse."
            li $ do
                strong "Sports"
                " must be heavily regulated."
                ol $ do
                    li "Attention must be taken to ensure that people do not cheat when playing monetised sports."
                    li "If sports are to be broken up into gender for reasons of strength and such, this split is to be done by hormone level, not by chromosomal sex."
            li $ do
                strong "Moving around"
                " is a basic right:"
                ol $ do
                    li "Everybody must be able to have freedom of movement."
            li $ do
                strong "Communication"
                " is a basic right, but subject to a couple of caveats:"
                ol $ do
                    li "Everybody must be able to communicate using the medium in which they wish, providing what they say doesn't infringe another's rights or make suggestions that may damage anothers' ."
                    li "Any one communication centre must not be artifically and deliberately impaired to the point that such functions are taken hostage."
                    li "Citizen communications are both private and secret, and must not be surveilled or monitored for any reason, and might as well all be end-to-end encrypted. This is also due to the fact that states have abused the trust of citizens."
            li $ do
                strong "Entertainment and information"
                " needs to be looked at carefully:"
                ol $ do
                    li "Everybody must be able to be entertained and informed correctly, in such a way that does not violate anybody else's rights."
                    -- Net neutrality
                    li "There must be no source which through any medium whose ability to inform is deliberately impaired for the purpose of devaluing or dissuading use in lieu of another."
            li $ do
                strong "Authority"
                " must be tightly controlled:"
                ol $ do
                    li "Those who believe that they are in control of a group of people, but abuse their trust by doing things that deny them their rights must be removed from their position of power."
                    li "Such places such tyrannical rulers currently terrorise must be influenced to remove them."
            li $ do
                strong "Nations"
                " should eventually be dissolved, and until then:"
                ol $ do
                    li "Monarchies are outdated and expensive, and are formed of the un-voted, and must be removed."
                    li "At least a sensible, actually democratic republic should be formed instead the nations currently being a monarchy."
            li $ do
                "Finally, "
                strong "voting and laws"
                " should be fair:"
                ol $ do
                    li "Being able to vote is a basic right that is only disqualifiable when convicted and proven of violent crimes."
                    li "Laws must make sure that the citizen is the primary recipient of help."
                    li "All laws must be decided upon collectively by those they affect. This means making all laws changeable based on referendums. This also means leaders and parties don't matter, and moving to a system of arbitrators is recommended."
                    li "Until this can be achieved, moving immediately to a fairer voting scheme such as Single Transferable Vote is mandatory."
                    li "This should be a very much global thing, and allows countries to be abolished."
    {-
    p $ do
        h2 "Petitions"
        p "Some current petitions I'd like to share with everyone:"
        table ! class_ "table table-striped table-hover" $ do
            thead . tr $ do
                th "Status"
                th "To whom"
                th "Subject"
                th "Why"
            tbody $ do
                tr $ do
                    td "Successful"
                    td "Somebody"
                    td "Do something"
                    td "It's a good thing"
    -}
    -- TODO:
    -- Intelligence agencies no longer needed for what though
    -- EU -> USE makes sense only for transfer and balance of power.
    -- 
    -- Firstly elect people, then abolish the house of lords on the way to dismantling countries
    -- Ban political advertising, advertising in general is only for approved items
    -- MP term limits: until government dissolved:  Yes, term limits will increase performance and prevent corruption
    -- Regulation on social media
    -- Don't arm school teachers, just have pro security guards
    -- Protect whistleblowers
    -- Internet: Neutrality
    --  No, treat all traffic equally and continue the openness of the internet
    --  No, this would allow them to remove competition, create artificial scarcity, and increase prices
    -- Only vote for things that affect you
    -- asbos: Yes, but increase penalties so offenders take them more seriously
    -- Vax: Everyone including businesses and schools unless it kills them. It's not okay to have religious exception, it's mandatory
    -- Nuclear  energy: Can be looked after, fusion is better tho
    -- Increase immigration until we dissolve countries - accept all who aren't violent criminals
    -- Deportation: only if safe and serious
    -- Having to learn languages: Translate to other cultures. Embrace diversity
    -- Citizenship tests are nonsense
    -- Immigrant terrorism promotion: deport for crime or planning crime if safe
    -- Dual citizenship for now is perfectly fine
    -- Family joining, Migrant quota
    -- Keep the NHS "N" and keep it affordable for foreigners too
    -- Private companies shouldn't profit from healthcare at all
    -- Legalise tax and regulate drugs
    -- Test voters before allowing their vote
    -- Allow foreigners to vote as long as they contribute to the system
    -- People in public office need to show tax returns and public audits to keep them in check
    -- Disallow violent or fraudulent criminal politicians
    -- Money doesn't belong in politics
    -- Incentivise and require environmental impact minimisation
    -- Ban disposable and non-biodegradable materials
    -- Animal testing no more
    -- Keep hunts illegal
    -- Investigate impact of fracking
    -- GM: Ban patents
    -- Improve infrastructure rather  than incentivising relocation of companies
    -- Police military equipment: only extreme situations
    -- Replace police with unarmed community based responders for non-violent calls
    -- Only violent crimes disqualify the right to vote
    -- Release nonviolent crime prisoners
    -- Prisons are all public
    -- Terrorists, even abroad, should be captured and trialled.
    -- Give primary school students nutritionally high free school meals if they are part of a low-income family
    -- Abolish tuition fees
    -- Critical race theory: work towards not needing it obviously
    -- Schools: make the subjects usefoland life needing and allow different ways of learning and testing. Then standardise this specific curriculum and revise it every few years
    -- Decriminalise truancy based on reasons not to need it
    -- Public all schools. Ban charter schools (managed privately)
    -- Transport, renationalise rail
    -- Allow all striking
    -- Extra fast travel spending
    -- Remove tax loopholes
    -- Don't restrict child benefit
    -- Uphill tax rates depending on earnings
    -- Tax mansions
    -- Allow lower work weeks for stress or mental health
    -- The idea of unions is good
    -- Increase benefit spending
    -- Decrease military, ensure wealth tax and no evasion, and large business
    -- Internet being a right it should be subject to benefits, and good enough to be provided
    -- Don't test welfare recipients for drugs, it's not the government's business
    -- Bedroom tax unless disability involved
    -- Nationalise banks
    -- Offshore banks only if they live there
    -- Abolish inheritance tax
    -- Abolish non-domicile and pay tax on earnings outside of the country
    -- Ideally make tax worldwide the same
    -- The concept of being an illegal immigrant makes no sense
    --  Abolish national borders / open border policy?



    p $ do
        "According to "
        extLink "https://isidewith.com" "ISideWith"
        ", this makes me:"
        ol $ do
            li "89% Green"
            li "86% Labour"
            li "84% Lib Dem"
            li "38% Conservative"
        extLink "My Profile:" "https://uk.isidewith.com/profile/4747687379"
    p $ do
        "If there's anything you think I've missed, or wish to discuss any of the above issues (like there's something you think I've misunderstood, disagree on something and you think you could change my opinion, etc.), you can contact me on: "
        a ! href "mailto:websitemorals@dandart.co.uk" $ "the Website Morals contact email"