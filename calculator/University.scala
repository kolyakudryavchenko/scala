package zadacha2

 trait BaseTrait
{def isUkranian(x:String):Boolean = x.equals("Ukraine");}
class Birga( var token:Int, var fiat:Int)
{
  private var _token:Int=1250000000;
  private var _fiat:Int=fiat;
  
  def Token:Int=_token;
  def Fiat:Int=_fiat;
  
  override def toString():String=s"\nToken:$token,\nFiat:$fiat";
}
class Coshelec( var token:Int, var fiat:Int)
{
  override def toString():String=s"\nToken:$token,\nFiat:$fiat";
}
class Human(var name:String, var surname:String, var age:Int, var adress:Adress, var coshelec:Coshelec)
{
  private var _name:String=name;
  private var _surname:String=surname;
  private var _age:Int=age;
  private var _adress:Adress=adress;
  private var _coshelec:Coshelec = coshelec;
  
  def Name:String=_name;
  def Surname:String=_surname;
  def Age:Int=_age;
  def Adres:Adress=_adress;
  def Birga:Coshelec=_coshelec;
  
  override def toString():String=s"Name:$name,\nSurname:$surname,\nAge:$age,$adress,$coshelec";
  
  def Show():String="------"+
  "\nName:"+name+
  "\nSurame:"+surname+
  "\nAge:"+age+
  "\nAdress:"+adress.toString();
}
class Adress(var country:String, var city:String, var house:Int)
{
  override def toString():String=s"\nCountry:$country,\nCity:$city,\nHouse:$house";
}
class Student(name:String, surname:String, age:Int, adress:Adress, coshelec:Coshelec, group:Int) extends Human(name,
    surname, age, adress, coshelec)with BaseTrait 
{
  private var _group:Int = group;
  
  def Group:Int =_group;

  override def toString():String=super.toString()+",\nGroup:"+_group ;
}
class Teacher(name:String, surname:String, age:Int, adress:Adress, coshelec:Coshelec, costCourse:Int) extends Human(name,
    surname, age, adress, coshelec) with BaseTrait
{
  private var _costCourse:Int=costCourse;
  
  def CostCourse:Int=_costCourse;
  
  override def toString():String=super.toString()+",\nCost Course:"+_costCourse;
}

object University {
  def main(args: Array[String]): Unit = 
  {
    var adress1=new Adress("Ukraine","Kherson",17);
    var adress2=new Adress("Ukraine","Kherson",9);
    var adress3=new Adress("Ukraine","Kherson",21);
    
    var coshelec1=new Coshelec(0,0);
    
    var human=new Human("Dasha","Inavova", 18, adress1, coshelec1);
    
    var student1=new Student("Mukola","Kydravchenko", 18, adress2, coshelec1, 241);
    var student2=new Student("Dasha","Ivanova", 18, adress2, coshelec1, 241);
    var student3=new Student("Bogdan","Sypryn", 18, adress2, coshelec1, 241);
    var student4=new Student("Olesya","Tantsyrenko", 18, adress2, coshelec1, 241);
    var student5=new Student("Danil","Avramenko", 18, adress2, coshelec1, 241);
    var student6=new Student("Amina","Hasimova", 18, adress2, coshelec1, 241);
    
    var teacher1=new Teacher("Franc","Joo", 54, adress3, new Coshelec(500,0), 500);
    var teacher2=new Teacher("Vera","Bon", 54, adress3, new Coshelec(350,0), 350);

    
    //println(human.toString()+"\n-------");
    //println(student.toString());
    //println(teacher.toString());
    //var mark1=scala.util.Random.nextInt(100).toString
    //println(student1.toString()+"\nMark:"+scala.util.Random.nextInt(100).toString+"\n-----------")
    //var mark=scala.util.Random.nextInt(100).toString
    //println(student2.toString()+"\nMark:"+scala.util.Random.nextInt(100).toString+"\n-----------")
   


   var listStudents1:List[Student]=List(student3,student5,student6);
   var listStudents2:List[Student]=List(student1,student2,student4);
   var cost1:Int=500;
   var cost2:Int=350;
   
   println(teacher1.toString()+",\nList Students:"+"\n");
   AllInfo(listStudents1, cost1);
   
   println(teacher2.toString()+",\nList Students:"+"\n");
   AllInfo(listStudents2, cost2);
}


  def AllInfo(list:List[Student], cost:Int):Unit=
  {
    for(i <- 0 until list.length)
    {
      println(list(i).toString());
      Marks(cost);
    }
  }

  def Marks(cost:Int):Unit=
  {
    var calc:Double=0;
   var credit:Double=0;
    var a: Int =1;
      while ( a <= 6)
      {
        var mark:Int=scala.util.Random.nextInt(100)
        println("Mark"+a+":"+mark);
        if ( 0 <=mark && mark <= 34)
        {
          println("За п'яти бальною системою "+mark+" це 1")
          calc=cost * 0
          credit=calc-cost
          println("Student Stipend:" + credit)
          println("Вам потрібно взяти " + credit * -1 + " токенів на біржі, щоб заплатити за навчання"+ "\n")
        }
          
        else if ( 35 <= mark && mark <= 59)
        {
          println("За п'яти бальною системою "+mark+" це 2")
          calc=cost*0.4
          credit=calc-cost
          println("Student Stipend:"+credit)
          println("Вам потрібно взяти "+credit*-1+" токенів на біржі, щоб заплатити за навчання"+"\n")
        }
          
        else if ( 60<=mark&&mark<=74)
        {
          println("За п'яти бальною системою"+mark+"це 3")
          calc=cost*0.8
          credit=calc-cost
          println("Student Stipend:"+credit)
          println("Вам потрібно взяти "+credit*-1+" токенів на біржі, щоб заплатити за навчання"+"\n")
        }
          
        else if ( 75 <= mark && mark <= 89)
        {
          println("За п'яти бальною системою "+mark+" це 4" )
          calc=cost*1
          credit=cost-calc
          println("Student Stipend:"+credit)
          println(credit+" Вам не треба патити за навчання"+"\n")
        }
          
        else if ( 90<=mark&&mark<=100)
        {
          println("За п'яти бальною системою "+mark+"це 5")
          calc=cost*1.2
          credit=calc-cost
          println("Student Stipend:"+credit)
          println("Вам надійшла стипендія "+credit+" токенів"+"\n")
        }
          
        a=a+1;
       }   
  }
  

 
}