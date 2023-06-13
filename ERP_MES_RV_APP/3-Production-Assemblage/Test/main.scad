use  <sidewall.scad>;
use  <windowhole.scad>;


linear_extrude(20){
    difference(){
        sidewall();
        window(50,35,100,50,30,30,15,15,25,25,20,20);
    }
}
