 Questions for MAK and MH

 1. PM2.5. We had discussed regressing PM2.5 on NO2 and adjusting for the residuals
    Upon adding new weather vars, now PM2.5 is driving missingness. I'm
    leaning towards not adjusting for PM2.5 and only doing so as a sens. analysis
    - adjust for non-traffic pm (regressing on the residuals) in a sens analysis
      only
 2. [Markus]Wind direction. In my committee meeting, we had discussed using a cyclic
    cubic spline, but then I think in future discussions we may have moved towards
    an 8-level categorical variable. Current models use the 8-level category.
    Should I try a cubic spline instead? Or do a sens analysis with this?
    - MAK noted she thinks the categorical variable is fine, and since this
      var isn't a big player tweaking how it is modeled isn't super important,
      but she said to defer to Markus's thoughts
    - MH agrees, 8 level cat is fine
 3. Model fit. Adding random slopes to the random intercepts model only very
    slightly improves model fit. I'm not sure which to choose as the main model.
    If we choose the random intercepts only model, we would essentially report that
    there wasn't a lot of variation of the intervention by monitor, but if we use the
    random intercepts and random slopes, we would report the random slopes as these
    would indicate the effect of the intervention on that monitor -- correct?
    - for simplicity chose chose RI and not RIS becuase they were so similar
      and the random slopes were really tiny
    - in supplement provide forest plot with slope and cis of RI model
      and slope and cis of RIS model as well as the random slopes
    - MH agrees
 4. Planned sensitivity analyses. Are all these a go?
    - with PM2.5 (yes)
    - impute NO2 by mean of same year, season, day-of-week, hour (yes)
    - remove outliers with resids >3SD+mean (yes)
    - change start date of intervention to March 20
    - remove NJ monitors
 5. [Markus]Talk about what the big picture storyline is.
    - MAK is on board with current storyline
    - Just revise and we will rediscuss with full draft
 7. Model diagnostics.
    (2c) heteroscedasticity. I've tried to figure out what's up with the weird
         linear line, to no avail.. but maybe it has to do with detection limits?
         - we're just gonna leave this alone
    (2d) non-linearity. Add splines for wind speed and precip?
         - yes add ns for wind speed and squared precip
         - need to check plot again and rerun all models w new model
    (2i) [Markus] autocorrelation. Making sure we're ok with the remaining autocorrelation?
         - try running the simple autocorrelation model again
         - try adding the rho value



