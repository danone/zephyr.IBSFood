#' @title Compute the nutriscore
#'
#' @description this function allows to compute the nutriscore from nutrients from food
#'
#' @param cal energy kJ
#' @param fat Saturated fatty acid g per 100g. default = 0
#' @param carbohydrate saccharides g per 100g. default = 0
#' @param protein protein g per 100g. default = 0
#' @param fibre fibre g per 100g. default = 0
#' @param sodium sodium mg per 100g. default = 0
#' @param fruit fruit g per 100g. default = 0
#' @param type drink or fat or others. default "others"
#'
#' @return a numeric value. the nutriscore
#' @examples
#' nutriscore(cal=507,fat=24,carbohydrate=65,fibre=2.7,sodium=600)
#'


nutriscore = function(
  cal=0,
  fat=0,
  carbohydrate=0,
  protein=0,
  fibre= 0,
  sodium = 0,
  fruit = 0,

  type=c("others")

) {


  score_neg = 0
  score_pos = 0
  score_fruit = 0
  score_fibre = 0

  if(type == "drink"){
    if (cal <= 0){
      score_neg = score_neg + 0;
    }
    else if (cal > 0 && cal <= 30){
      score_neg = score_neg + 1;
    }
    else if (cal > 30 && cal <= 60){
      score_neg = score_neg + 2;
    }
    else if (cal > 60 && cal <= 90){
      score_neg = score_neg + 3;
    }
    else if (cal > 90 && cal <= 120){
      score_neg = score_neg + 4;
    }
    else if (cal > 120 && cal <= 150){
      score_neg = score_neg + 5;
    }
    else if (cal > 150 && cal <= 180){
      score_neg = score_neg + 6;
    }
    else if (cal > 180 && cal <= 210){
      score_neg = score_neg + 7;
    }
    else if (cal > 210 && cal <= 240){
      score_neg = score_neg + 8;
    }
    else if (cal > 240 && cal <= 270){
      score_neg = score_neg + 9;
    }
    else if (cal > 270){
      score_neg = score_neg + 10;
    }
    #console.log("calorie: " + score_negatif);

  }

  else{
    if (cal <= 335){
      score_neg = score_neg + 0;
    }
    else if (cal > 335 && cal <= 670){
      score_neg = score_neg + 1;
    }
    else if (cal > 670 && cal <= 1005){
      score_neg = score_neg + 2;
    }
    else if (cal > 1005 && cal <= 1340){
      score_neg = score_neg + 3;
    }
    else if (cal > 1340 && cal <= 1675){
      score_neg = score_neg + 4;
    }
    else if (cal > 1675 && cal <= 2010){
      score_neg = score_neg + 5;
    }
    else if (cal > 2010 && cal <= 2345){
      score_neg = score_neg + 6;
    }
    else if (cal > 2345 && cal <= 2680){
      score_neg = score_neg + 7;
    }
    else if (cal > 2680 && cal <= 3015){
      score_neg = score_neg + 8;
    }
    else if (cal > 3015 && cal <= 3350){
      score_neg = score_neg + 9;
    }
    else if (cal > 3350){
      score_neg = score_neg + 10;
    }
    #console.log("calorie: " + score_negatif);

  }

  #// saturated fat
  if(type == "fat"){
    if (fat <= 6){
      score_neg = score_neg + 0;
    }
    else if (fat > 6 && fat <= 10){
      score_neg = score_neg + 1;
    }
    else if (fat > 10 && fat <= 14){
      score_neg = score_neg + 2;
    }
    else if (fat > 14 && fat <= 18){
      score_neg = score_neg + 3;
    }
    else if (fat > 18 && fat <= 22){
      score_neg = score_neg + 4;
    }
    else if (fat > 22 && fat <= 26){
      score_neg = score_neg + 5;
    }
    else if (fat > 26 && fat <= 30){
      score_neg = score_neg + 6;
    }
    else if (fat > 30 && fat <= 34){
      score_neg = score_neg + 7;
    }
    else if (fat > 34 && fat <= 38){
      score_neg = score_neg + 8;
    }
    else if (fat > 38 && fat <= 42){
      score_neg = score_neg + 9;
    }
    else if (fat > 42){
      score_neg = score_neg + 10;
    }
    #console.log("gras: " + score_negatif);

  }
  else{
    if (fat <= 1){
      score_neg = score_neg + 0;
    }
    else if (fat > 1 && fat <= 2){
      score_neg = score_neg + 1;
    }
    else if (fat > 2 && fat <= 3){
      score_neg = score_neg + 2;
    }
    else if (fat > 3 && fat <= 4){
      score_neg = score_neg + 3;
    }
    else if (fat > 4 && fat <= 5){
      score_neg = score_neg + 4;
    }
    else if (fat > 5 && fat <= 6){
      score_neg = score_neg + 5;
    }
    else if (fat > 6 && fat <= 7){
      score_neg = score_neg + 6;
    }
    else if (fat > 7 && fat <= 8){
      score_neg = score_neg + 7;
    }
    else if (fat > 8 && fat <= 9){
      score_neg = score_neg + 8;
    }
    else if (fat > 9 && fat <= 10){
      score_neg = score_neg + 9;
    }
    else if (fat > 10){
      score_neg = score_neg + 10;
    }
    #console.log("gras: " + score_negatif);
  }
  #// sucre

  if(type == "drink"){
    if (carbohydrate <= 0){
      score_neg = score_neg + 0;
    }
    else if (carbohydrate > 0 && carbohydrate <= 1.5){
      score_neg = score_neg + 1;
    }
    else if (carbohydrate > 1.5 && carbohydrate <= 3){
      score_neg = score_neg + 2;
    }
    else if (carbohydrate > 3 && carbohydrate <= 4.5){
      score_neg = score_neg + 3;
    }
    else if (carbohydrate > 4.5 && carbohydrate <= 6){
      score_neg = score_neg + 4;
    }
    else if (carbohydrate > 6 && carbohydrate <= 7.5){
      score_neg = score_neg + 5;
    }
    else if (carbohydrate > 7.5 && carbohydrate <= 9){
      score_neg = score_neg + 6;
    }
    else if (carbohydrate > 9 && carbohydrate <= 10.5){
      score_neg = score_neg + 7;
    }
    else if (carbohydrate > 10.5 && carbohydrate <= 12){
      score_neg = score_neg + 8;
    }
    else if (carbohydrate > 12 && carbohydrate <= 13.5){
      score_neg = score_neg + 9;
    }
    else if (carbohydrate > 13.5){
      score_neg = score_neg + 10;
    }
    #console.log("sucre: " + score_negatif);

  }

  else{
    if (carbohydrate <= 4.5){
      score_neg = score_neg + 0;
    }
    else if (carbohydrate > 4.5 && carbohydrate <= 9){
      score_neg = score_neg + 1;
    }
    else if (carbohydrate > 9 && carbohydrate <= 13.5){
      score_neg = score_neg + 2;
    }
    else if (carbohydrate > 13.5 && carbohydrate <= 18){
      score_neg = score_neg + 3;
    }
    else if (carbohydrate > 18 && carbohydrate <= 22.5){
      score_neg = score_neg + 4;
    }
    else if (carbohydrate > 22.5 && carbohydrate <= 27){
      score_neg = score_neg + 5;
    }
    else if (carbohydrate > 27 && carbohydrate <= 31){
      score_neg = score_neg + 6;
    }
    else if (carbohydrate > 31 && carbohydrate <= 36){
      score_neg = score_neg + 7;
    }
    else if (carbohydrate > 36 && carbohydrate <= 40){
      score_neg = score_neg + 8;
    }
    else if (carbohydrate > 40 && carbohydrate <= 45){
      score_neg = score_neg + 9;
    }
    else if (carbohydrate > 45){
      score_neg = score_neg + 10;
    }
    #console.log("sucre: " + score_negatif);
  }


  #// sodium
  if (sodium <= 90){
    score_neg = score_neg + 0;
  }
  else if (sodium > 90 && sodium <= 180){
    score_neg = score_neg + 1;
  }
  else if (sodium > 180 && sodium <= 270){
    score_neg = score_neg + 2;
  }
  else if (sodium > 270 && sodium <= 360){
    score_neg = score_neg + 3;
  }
  else if (sodium > 360 && sodium <= 450){
    score_neg = score_neg + 4;
  }
  else if (sodium > 450 && sodium <= 540){
    score_neg = score_neg + 5;
  }
  else if (sodium > 540 && sodium <= 630){
    score_neg = score_neg + 6;
  }
  else if (sodium > 630 && sodium <= 720){
    score_neg = score_neg + 7;
  }
  else if (sodium > 720 && sodium <= 810){
    score_neg = score_neg + 8;
  }
  else if (sodium > 810 && sodium <= 900){
    score_neg = score_neg + 9;
  }
  else if (sodium > 900){
    score_neg = score_neg + 10;
  }
  #console.log("sodium: " + score_negatif);

  #// points positifs

  #// fruits et légumes


  if(type == "drink"){

    if (fruit <= 40){
      score_pos = score_pos + 0;
      score_fruit = 0;
    }
    else if (fruit > 40 && fruit <= 60){
      score_pos = score_pos + 2;
      score_fruit = 1;
    }
    else if (fruit > 60 && fruit <= 80){
      score_pos = score_pos + 4;
      score_fruit = 2;
    }
    else if (fruit > 80){
      score_pos = score_pos + 10;
      score_fruit = 5;
    }

  } else {

  if (fruit <= 40){
    score_pos = score_pos + 0;
    score_fruit = 0;
  }
  else if (fruit > 40 && fruit <= 60){
    score_pos = score_pos + 1;
    score_fruit = 1;
  }
  else if (fruit > 60 && fruit <= 80){
    score_pos = score_pos + 2;
    score_fruit = 2;
  }
  else if (fruit > 80){
    score_pos = score_pos + 5;
    score_fruit = 5;
  }

  }
  #console.log("fruit: " + score_positif);

  #// fibres
  if (fibre <= 0.7){
    score_pos = score_pos + 0;
    score_fibre = 0;
  }
  else if (fibre > 0.7 && fibre <= 1.4){
    score_pos = score_pos + 1;
    score_fibre = 1;
  }
  else if (fibre > 1.4 && fibre <= 2.1){
    score_pos = score_pos + 2;
    score_fibre = 2;
  }
  else if (fibre > 2.1 && fibre <= 2.8){
    score_pos = score_pos + 3;
    score_fibre = 3;
  }
  else if (fibre > 2.8 && fibre <= 3.5){
    score_pos = score_pos + 4;
    score_fibre = 4;
  }
  else if (fibre > 3.5){
    score_pos = score_pos + 5;
    score_fibre = 5;
  }
  #console.log("fibre: " + score_positif);

  #// proteines
  if (protein <=1.6){
    score_pos = score_pos + 0;
  }
  else if (protein > 1.6 && protein <= 3.2){
    score_pos = score_pos + 1;
  }
  else if (protein > 3.2 && protein <= 4.8){
    score_pos = score_pos + 2;
  }
  else if (protein > 4.8 && protein <= 6.4){
    score_pos = score_pos + 3;
  }
  else if (protein > 6.4 && protein <= 8.0){
    score_pos = score_pos + 4;
  }
  else if (protein > 8.0){
    score_pos = score_pos + 5;
  }
  #console.log("proteine: " + score_positif);

  if(score_neg < 11) {

  score_final = score_neg - score_pos;
  }
  else if (score_neg >= 11 && score_fruit == 5) {

    score_final = score_neg - score_pos;

  }
  else if (score_neg >= 11 && score_fruit < 5) {

    score_final = score_neg - (score_fruit + score_fibre) ;

  }


  return(list(score_final=score_final,type=type))

  #to do -10-0-3-10-18

}


#' @title Compute the ABCDE nutriscore
#'
#' @param nutriscore a nutriscore object generated with nutriscore function. no default

#'
#' @return a character. the ABCDE nutriscore
#' @examples
#' nutriscore_qual(nutriscore=11)
#'


nutriscore_qual = function(nutriscore=NULL){


  if(nutriscore$type=="drink"){

    i=findInterval(nutriscore$score_final, c(-100,1,2,5,9,100))


  } else {

    i=findInterval(nutriscore$score_final, c(-100,0,3,10,18,100))
  }

  return(LETTERS[i])


}






