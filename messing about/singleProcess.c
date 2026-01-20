/*
#=======================================#
 MAIN MODEL SCRIPT
#=======================================#
 */
 

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <stdbool.h>

#include "CONSTANTS.h"
#include "getr.h"
#include "include/R.h"
#include "include/Rmath.h"

double ModelRun( 
double prey_abundance[InputLength*NoTaxa], 
double temp[InputLength] ,
double light[InputLength] ,
int prey_mode[NoTaxa],           
double prey_energy[NoTaxa] ,
double prey_ed[NoTaxa] ,
double prey_size[NoTaxa] ,   
double  prey_image_area[NoTaxa] ,    
double a_c[InputLength],
int JulianDayV[InputLength],
int DayLengths[InputLength], 
double R[NoInputs],
double S[NoInputs],
int JD_ADDED[NoInputs],
int JD_FINISH[NoInputs],
double LENGTH[NoInputs],
double WEIGHT[NoInputs],
int ModelRunLengths[NoInputs],
//int STRUCTsens[1],
double *result)

{
  

  
  
  // set up initial conditions 
  double LENGTH_0 = LENGTH[0];
  double WEIGHT_0 = WEIGHT[0];
  double S_0 = S[0];
  double R_0 = R[0];
  
  
  
  //~~~~~~~~~~~ ENVIRONMENTAL DATA VECTORS ~~~~~~~~~~~~//
  
//   // temperature-dependent metabolism coefficient
//   double METABOLISM_Q10_FEEDV[InputLength];
//   for( int i = 0; i < InputLength; i++) 
//     METABOLISM_Q10_FEEDV[i] =  M_FEED*pow(Q10_MF , temp[i] / 10) ;
  
//   // temperature-dependent gut evacuation coefficient
//   double GutEvacV[InputLength];
//   for( int i = 0; i < InputLength; i++) 
//     GutEvacV[i] = GE1*pow(M_E, (GE2*temp[i])); 
  
//   // temperature-dependent assimilation
//   double assimilationV[InputLength];
//   for( int i = 0; i < InputLength; i++)
//     assimilationV[i] = (A1 + A2*temp[i])-Ua;
  
  

//#=======================================#
  // MAIN LOOP
//#=======================================#

  

  
  // open a file to store results
  static const char filename97[] = "Individuals.txt";
  FILE *file97 = fopen ( filename97, "w" );
  
  
  int position = -1;

  double assimilation = 0.8;
  
  for( int ip = 0; ip < NoInputs; ip++) // loop through different environmental scenarios
  {
    
    
    // starting over with initial conditions if run for several different environmental scenarios
    double LENGTH = LENGTH_0;
    double WEIGHT = WEIGHT_0;
    double S = S_0;
    double R = R_0;

    
    //double stomach_content_start_next_day = 0; // stomach content is 0 at start of model run
    
    int NoDays = ModelRunLengths[ip];
    


    
    for( int t = 0; t < NoDays; t++) // loop through days
    {

    
      position = position + 1;
      
      
      // load day-specific values
      int JulianDay =  JulianDayV[position];                           // julian day
      //double METABOLISM_Q10_FEED = METABOLISM_Q10_FEEDV[position];     // metabolism coefficient
      //double GutEvac = GutEvacV[position];                             // gut evacuation coefficient
      //double assimilation = assimilationV[position];                   // assimilation coefficient
      int h_feed = DayLengths[position] - 2;                         // feeding period (hours of light - 2h for ascent/descent)
      
      // ~~~ INGESTION ~~~ //
      
      
      // MAX INTAKE
      
      // initialising variables for functional response
      double numerator_energy[NoModes]; double numerator_weight[NoModes]; double numerator_size[NoModes];  double numerator_number[NoModes];  double denominator[NoModes];
      
      double avg_prey_size = 0; double total_abundance = 0;
      
   
      
      // calculating numerators and denominators for obtaining max intake rates
      for(int p = 0; p < NoTaxa; p++)
      {
        
        double ac = a_c[position];
        
        double E = (   pow(( (LENGTH / 100 )*dec_dist_scale),2) )/(  C *  (  (  pow(10,  (2.62 *log10( 7 ) -2.01) )  )/1000000   )   ); // eye sensitivity 
        double detection_distance = sqrt( (prey_image_area[p]/1000000) * E * C * (light[position]/ (light[position]+kR) ) ) ; 
        double ab = (ac - 0.04)/0.2; // beam attenuation
        
        detection_distance = getr( detection_distance, 
                                   ab, 
                                   (prey_image_area[p]/1000000),
                                   E,
                                   light[position], 
                                        kR);
        

        
        
        //what does it mean when we square root prey image area smaller than 1?

    
        double search_rate = M_PI*pow(detection_distance,2)*swimming_speed*60*60 * ( (LENGTH )/100 ) ; 
        //double search_rate = M_PI*pow(detection_distance,2)*540 ; 
        
        // original NOOO
        //double efficiency = 0.5*(1-(1/(1+exp(-2*(log(prey_size[p] /((10.0)*LENGTH) )-7))))); // size-ratio dependent capture efficency
        
        // constant
        // double efficiency = 0.5;
        
        // linear with prey size
        // this is ok but very variable and FoF doing poorly
        //double efficiency =  0.4 + (-0.4999/17.0)*prey_size[p];
        //if(efficiency < 0 ) efficiency = 0;
        
        // nonlinear with prey size
        //double efficiency = 0.86*(1-(1/(1+exp(-b*(log(prey_size[p] /((10.0)) )-(-1)))))); // no decline in DB anymore, spend too much time in C
        //double efficiency = 0.86*(1-(1/(1+exp(-4.4*(log(prey_size[p] /((10.0)) )-(-0.5)))))); // not good
        //double efficiency = 0.86*(1-(1/(1+exp(-4.4*(log(prey_size[p] /((10.0)) )-(-2)))))); // ok but decline in DB not so clear
        
        // linear size ratio decline?
        //double efficiency = 0.5-(0.5/0.3)*(prey_size[p] /((10.0)*LENGTH))  ; // NOO shoots off
        //if(efficiency < 0 ) efficiency = 0;
        
         //double efficiency =  0;
        //if(prey_mode[p] == 1) efficiency = 1;
        //if(prey_mode[p] == 2) efficiency = 0;
        //if(prey_mode[p] == 3) efficiency = 0;
        
        // linear with prey size
        //double efficiency =  0.4 + (-0.4999/17.0)*prey_size[p] + 0.005*LENGTH; // doesn't help with most and makes some shoot off
        //if(efficiency < 0 ) efficiency = 0;

        // baseline
        //double efficiency =  1 + (-0.4999/17.0)*prey_size[p];
        //if(efficiency < 0 ) efficiency = 0;
        
        // THIS + handling time =  40 sec works
        //double efficiency = 1*(1-(1/(1+exp(-4.4* (log(prey_size[p] /10.0 ) -  (-2)  )  )))); // ok but decline in DB not so clear
        
        // with paras instead
        double efficiency = 1*(1-(1/(1+exp(-b* (log(prey_size[p] /10.0 ) -  m  )  )))); // ok but decline in DB not so clear
        

        double abundance = prey_abundance[position*NoTaxa + p]; // abundance of prey type
        double capture_rate = efficiency * search_rate * abundance; // capture rate ignoring handling time
        
        
        for(int m = 0; m < NoModes; m++){ // adding on to respective numerators/denominators if type matches mode

          numerator_energy[m] = numerator_energy[m] + capture_rate * prey_energy[p] * (prey_mode[p]==(m+1));
          numerator_weight[m] = numerator_weight[m] + capture_rate * prey_energy[p]/prey_ed[p] * (prey_mode[p]==(m+1));
          numerator_number[m] = numerator_number[m] + capture_rate * (prey_mode[p]==(m+1));   
          numerator_size[m] = numerator_size[m] + capture_rate * prey_size[p] * (prey_mode[p]==(m+1));  
          denominator[m] = denominator[m] + capture_rate * handling_time * (prey_mode[p]==(m+1));
         
        }
        
        total_abundance = total_abundance + abundance;
        avg_prey_size = avg_prey_size + abundance*prey_size[p];
        
      
      }
      

      // calculating max intake rates for each mode
      double max_intake[NoModes]; double max_intake_weight[NoModes]; double max_intake_number[NoModes]; double max_intake_size[NoModes]; double total_max = 0;
      
      double max_intake_numberA = 0;
      double max_intake_numberB = 0;
      double max_intake_numberC = 0;
      
      for(int m = 0; m < NoModes; m++){
        

        max_intake[m] = numerator_energy[m]/(1+denominator[m]);

        max_intake_weight[m] = numerator_weight[m]/(1+denominator[m]);
        max_intake_number[m] = numerator_number[m]/(1+denominator[m]);
        max_intake_size[m] = numerator_size[m]/(1+denominator[m]);
        total_max = total_max + max_intake[m] ; // this is across modes - used for determining relative profitability

        if(m == 0)  max_intake_numberA = max_intake_number[m];
        if(m == 1)  max_intake_numberB = max_intake_number[m];
        if(m == 2)  max_intake_numberC = max_intake_number[m];
        
        numerator_energy[m] = 0;
        numerator_weight[m] = 0;
        numerator_number[m] = 0;   
        numerator_size[m] = 0;  
        denominator[m] = 0;


      }
      

      
      
      // ACTUAL INTAKE (LIMITED BY STOMACH CAPACITY)
      
      
      // initialise variables that track total ingested energy and total ingested weight
      double total_intake = 0;
      double total_intake_w = 0;
      double avg_stomach_content = 0; // this is to keep track of average stomach content during feeding
      double selectivity = 0;
      double ingested_prey_size = 0;
      
      double mode_1 = 0;  double mode_2 = 0;  double mode_3 = 0;
      
      // calculating maximum ingested energy per hour (assuming the sandeels spend time in each mode in proportion to the profitability of each mode)
      if(total_max != 0) // only runs if there is food around, otherwise intake stays at 0
      {
        double i_max = 0; double i_max_weight = 0; double i_max_number = 0; double i_max_size = 0;
        
        for(int m = 0; m < NoModes; m++){

          i_max = i_max + (max_intake[m]/total_max)  *  max_intake[m];
          i_max_weight = i_max_weight + (max_intake[m]/total_max)  *  max_intake_weight[m];
          i_max_number = i_max_number + (max_intake[m]/total_max)  *  max_intake_number[m];
          i_max_size = i_max_size + (max_intake[m]/total_max)  *  max_intake_size[m];
          if(m == 0) mode_1 = (max_intake[m]/total_max);
          if(m == 1) mode_2 = (max_intake[m]/total_max);
          if(m == 2) mode_3 = (max_intake[m]/total_max);
          

        }
        


        // selectivity
        
        selectivity = (i_max_size/i_max_number) / (avg_prey_size/total_abundance);
        ingested_prey_size = (i_max_size/i_max_number);
        
        
        // energy density of intake
        
        double intake_ed = i_max/i_max_weight;

        // keeping track of stomach content
       // double stomach_content = stomach_content_start_next_day; // 0 if first day, otherwise saved since end of feeding period day before

        
        // maximum stomach content
       // double max_stomach = alpha_stomach*pow(LENGTH, beta_stomach); 
        
      

        // loop through all hours of feeding - update stomach content each hour
        for( int h = 0; h < h_feed; h++)  
        {

          // if there is enough space to feed at maximum rate
        //   if(max_stomach-stomach_content >= i_max_weight)
        //   {

            
            total_intake = total_intake + i_max;
        //     total_intake_w = total_intake_w + i_max_weight;
        //     stomach_content = stomach_content - stomach_content*GutEvac*(GE3/intake_ed) + i_max_weight;

            
        //   }
          
        //   // if limited by stomach space
        //   if(max_stomach-stomach_content < i_max_weight)
        //   {
        //     total_intake = total_intake + (max_stomach-stomach_content)*intake_ed;
        //     total_intake_w = total_intake_w + max_stomach-stomach_content;
        //     stomach_content = stomach_content - stomach_content*GutEvac*(GE3/intake_ed) + max_stomach-stomach_content;
          

          
        //   }
          
        //   avg_stomach_content = avg_stomach_content + stomach_content;

        }
        

        
        //avg_stomach_content = avg_stomach_content/h_feed; 
        
        

        
        //stomach_content_start_next_day = stomach_content*pow((1-GutEvac*(GE3/intake_ed)), 24-h_feed); // remove what can be digested until next feeding period

        

        total_intake = total_intake*assimilation; // account for assmilation efficiency
        total_intake = total_intake/1000; // R and S are in kJ and rest is in J
        
      }

 
    //   if(total_intake == 0) // if there is no food - assume that stomach will be empty at start of next day
    //   {
    //     stomach_content_start_next_day = 0;
    //   }
      

      
      // ~~~ CALCULATING AND ALLOCATING NET ASSIMILATED ENERGY ~~~ //
      
      
      // METABOLISM
    //   double MET_SMR = (pow(WEIGHT,rrr)*METABOLISM_Q10_FEED); // standard metabolic cost for 24h 
    //   double MET_FEED = WEIGHT*(h_feed+2)*FEED_COST; // active during all hours of light if feeding
    //   double MET_SDA = (total_intake/assimilation)*SDA; // cost of processing food
      
      
      // NET ENERGY GAIN
      double NET_A;
      NET_A = total_intake;

      
    //   if(SWITCH != 2){ // standard model assuming feeding only takes place if costs are met
    //     if((MET_FEED + MET_SDA) >= total_intake) // do not feed if costs are not met
    //     {
    //       NET_A  =  - MET_SMR;
    //     }   
        
    //     if((MET_FEED + MET_SDA) < total_intake) // feed if costs are met
    //     {
    //       NET_A = total_intake - (MET_FEED + MET_SMR + MET_SDA);
    //     }   
    //   }
      
    //   if(SWITCH == 2){ // checking assumption
    //     NET_A = total_intake - (MET_FEED + MET_SMR + MET_SDA);
        
    //   }
      
      
    //   // ENERGY ALLOCATION
    //   if(NET_A > 0)
    //   {
        
    //     if(SWITCH != 4){
          
    //       if(S/(R+S + NET_A) < (  S/ ( pow(  S/(alloc_a/(alloc_b+1))    , 1/(alloc_b+1)) ) ) ){ // "ideal ratio" based on ratio before starvation
            
    //       S = S + ((alloc_a*pow(S+R, alloc_b))*NET_A) ;  // NEW STRUCTURAL ENERGY
    //       R = R + (1 - (alloc_a*pow(S+R, alloc_b)))*NET_A ; // NEW RESERVE ENERGY
          
    //       }
          
    //       else{
    //         R = R + NET_A;
    //       }

        
    //     }
        
    //     if(SWITCH == 4){
    //       S = S + ((alloc_a*pow(S+R, alloc_b))*NET_A) ;  // NEW STRUCTURAL ENERGY
    //     }
        
    //   }
      
    //   else
    //   {
       
    //     R = R + NET_A; // pay metabolic costs from reserves
    //   }
      
      S = S + ((alloc_a*pow(S+R, alloc_b))*NET_A) ;  // NEW STRUCTURAL ENERGY
      R = R + (1 - (alloc_a*pow(S+R, alloc_b)))*NET_A ; // NEW RESERVE ENERGY
      

      // ~~~ UPDATING LENGTH AND WEIGHT AND SAVING OUTPUT ~~~ //
      

      // UPDATE LENGTH AND WEIGHT
      

  
      
      LENGTH = pow(S/(alpha_S*Es), (1/(beta_S))); // NEW LENGTH
      WEIGHT = (R/Er)*Rdw + (S/Es)*Sdw ; // NEW WEIGHT
      
      
      // STORING OUTPUT
      fprintf(file97, "%d %d  %G %G %G %G %G %G %G %G %G %G %G %G %G %G %G \n",ip, JulianDay,  R,S, WEIGHT, LENGTH, total_intake_w, total_intake, selectivity, ingested_prey_size, mode_1, mode_2, mode_3, max_intake_numberA, max_intake_numberB, max_intake_numberC) ;
      fflush(file97);
      
      
 
      
    }
    
  //printf("Number: %d\n", ip);
 
  }
  
  
  
  fclose(file97) ;
  return( *result) ;
  

  
}


