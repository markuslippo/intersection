package intersection

import scala.swing._
import java.awt.{Color, Graphics2D}
import java.awt.event.ActionListener
import java.io.File
import javax.imageio.ImageIO
import scala.swing.event._
import javax.swing.UIManager


object SimulationApp extends SimpleSwingApplication {

  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)

   /**
   *    A variable used for keeping track of time.
   */

  var seconds = 1

   /**
   *    The background image and details of width and heigth
   */

  val backgroundImage2 = ImageIO.read(new File("pictures/Intersection.ready.png"))
  val width = 1471
  val heigth = 967
  val fullHeigth = 1000

  Writer.readMyFile("dataFromFile.txt")
  val parameters = Writer.parseParameters

   /**
   *    The user has 3 button options: Spawning a car, quitting the simulation
   *    and putting the simulation on pause.
   */


  var onPause = false
  var techModeOn = false

  val userControlled = parameters(1) == 1
  var autoSpawnOn = !userControlled

  val HorizontalButton = new Button("Green light horizontal") {
   listenTo(mouse.clicks)
   reactions += {
     case clickEvent: MouseClicked =>
       Intersection.lightSystem.verticalGreenLight = false
       Intersection.lightSystem.horizontalGreenLight = true
       Intersection.lightSystem.horizontal.foreach(_.changeState(1))
       Intersection.lightSystem.vertical.foreach(_.changeState(0))
   }
  }

  val VerticalButton = new Button("Green light vertical") {
   listenTo(mouse.clicks)
   reactions += {
     case clickEvent: MouseClicked =>
       Intersection.lightSystem.verticalGreenLight = true
       Intersection.lightSystem.horizontalGreenLight = false
       Intersection.lightSystem.horizontal.foreach(_.changeState(0))
       Intersection.lightSystem.vertical.foreach(_.changeState(1))
   }
  }

  val NorthButton = new Button("North") {
    listenTo(mouse.clicks)
    reactions += {
      case clickEvent: MouseClicked => Intersection.createCar(spawnDirection.North)
    }
  }

  val SouthButton = new Button("South") {
    listenTo(mouse.clicks)
    reactions += {
      case clickEvent: MouseClicked => Intersection.createCar(spawnDirection.South)

    }
  }
  val EastButton = new Button("East") {
    listenTo(mouse.clicks)
    reactions += {
      case clickEvent: MouseClicked => Intersection.createCar(spawnDirection.East)

    }
  }
  val WestButton = new Button("West") {
    listenTo(mouse.clicks)
    reactions += {
      case clickEvent: MouseClicked => Intersection.createCar(spawnDirection.West)

    }
  }

  val spawnRandom = new Button("Spawn") {
    listenTo(mouse.clicks)
    reactions += {
      case clickEvent: MouseClicked => Intersection.createRandomCar()
    }
  }

  val autospawn = new Button("Autospawn") {
    listenTo(mouse.clicks)
    reactions += {
      case clickEvent: MouseClicked => autoSpawnOn = !autoSpawnOn
    }
  }

  val techButton = new Button("Tech View") {
    listenTo(mouse.clicks)
    reactions += {
      case clickEvent: MouseClicked => techModeOn = !techModeOn
    }
  }

  val pauseButton = new Button("Pause") {
    listenTo(mouse.clicks)
    reactions += {
      case clickEvent: MouseClicked => onPause = !onPause
    }
  }

  val quitButton = new Button("Quit") {
   listenTo(mouse.clicks)
   reactions += {
     case clickEvent: MouseClicked =>
        Writer.writeToFile("dataFromSimulation.txt", false)
        System.exit(0)
   }
  }

   /**
   *     Create a BoxPanel with horizontal orientation, aka
   *     the buttons are on the left corner of the Window
   *     in a line.
   */

  val Buttons = new BoxPanel(Orientation.Horizontal) {
      if(userControlled) {
      contents += spawnRandom
      }
      contents += techButton
      contents += pauseButton
      if(userControlled) {
      contents += autospawn
      contents += NorthButton
      contents += SouthButton
      contents += EastButton
      contents += WestButton
      }
      if(userControlled) {
      contents += HorizontalButton
      contents += VerticalButton
      }
      contents += quitButton

      contents += Swing.VStrut(20)
    }
  /**
   *     This is the MainFrame. It is not resizable.
   */

    def top = new MainFrame {
    title     = "Intersection"
    resizable = false

    minimumSize = new Dimension(width, fullHeigth)
    preferredSize = new Dimension(width, fullHeigth)
    maximumSize = new Dimension(width, fullHeigth)

  /**
   *    BackGround panel. It has calls two different draws:
   *    one on the background picture and one on Intersection.draw
   */

    val BackGround = new Panel {
      override def paintComponent(g: Graphics2D) = {
        g.drawImage(backgroundImage2, 0, 0, null)
        Intersection.draw(g)
        g.setColor(Color.red)

        if(techModeOn) {
        g.drawRect(350, 500, 200, 150) //West
        g.drawRect(950, 325, 200, 150) //East
        g.drawRect(750, 675, 150, 200) //South
        g.drawRect(575, 75, 150, 200) //North
        g.setColor(Color.PINK)
        g.drawRect(575, 300, 350, 350)

        }
      }
    }

   /**
   *     Add a BoxPanel to Mainframe with Vertical Orientation. This means that
    *    the buttons will be under the "Simulation Window".
   */

    contents = new BoxPanel(Orientation.Vertical) {
    contents += BackGround
    contents += Buttons
    }

    /**
    *    ActionListener for forwarding the Intersection.
    *    This is called many times in a second.
    */

    val IntersectionForward = new ActionListener(){
      def actionPerformed(e : java.awt.event.ActionEvent) = {

      if(!onPause) {
      if(Intersection.carCrashHappened) {
              quit()
              Dialog.showMessage(top, "", "Simulation ended due to a car crash.")

      }

      Intersection.step()
      BackGround.repaint()
        }
     }
    }

    /**
    *     ActionListener for forwarding the TrafficLightSystem.
    *     This is only called every second.
    */

    val LightSystemForward = new ActionListener() {
     def actionPerformed(e : java.awt.event.ActionEvent) = {
      if(!onPause) {
        seconds += 1
        if(!userControlled)
          Intersection.lightsStep()

      if(seconds % parameters.head == 0 && autoSpawnOn)
        Intersection.createRandomCar()
      }
     }
    }

     /**
      *  Timers for both IntersectionForward and LightSystemForward,
      *  respectively
      */
      val timer = new javax.swing.Timer(6, IntersectionForward)
     timer.start()

     val timeClock = new javax.swing.Timer(1000, LightSystemForward)
     timeClock.start()
    }
  }


