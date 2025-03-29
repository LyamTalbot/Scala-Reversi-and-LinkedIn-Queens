//> using platform scala-js
//> using jsVersion 1.18.2
//> using dep "com.wbillingsley::doctacular::0.3.0"

package assignmentTwo.ui


import com.wbillingsley.veautiful.html.*  
import com.wbillingsley.veautiful.doctacular.* 

val site = Site()
given styles:StyleSuite = StyleSuite()

def home = <.article(
    <.h2("UI for assignment 2"),
    <.p("This is a little UI to let you interact with your code in assessment 2. Use the links in the left side-bar to go to the different puzzles.")
)

@main def main() = 
    println("Loaded script, running...")
    import site.given

    site.toc = site.Toc(
        ("Home" -> site.HomeRoute),  
        ("Reversi" -> site.addPage("reversi", reversiPage)),
        ("LinkedIn Queens" -> site.addPage("queens", queensPage))
    )

    site.home = () => site.renderPage(home)

    styles.install()    
    site.attachToBody()

