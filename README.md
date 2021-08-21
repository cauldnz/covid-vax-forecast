# covid-vax-forecast
A basic model for performing logistic growth forecast of Covid vaxs via Prophet. It's pretty scruffy and really just for working out my chances to ski in Japan this winter and for arguing on Twitter. But may be useful for others to grab and iterate on...

I am a fan of the http://covidvax.live site. But, from what I can tell it's just straight line projecting recent growth out in order to build its forecast for when countries reach 70%. I'm not convinced this is quite as realistic as we can be given it's a constrained growth (logistic growth) model. So here's my attempt at using the same data set for a quick and dirty whip-up of a Prophet based growth model.

Some example output
- https://cauldnz.github.io/covid-vax-examples/AUS-forecast.html
- https://cauldnz.github.io/covid-vax-examples/JPN-forecast.html
- https://cauldnz.github.io/covid-vax-examples/NZL-forecast.html

If you're having trouble using Prophet on Ubuntu (and maybe other flavours of Linux/Unix) then you're likely running into an issue building RStan. Check that you have a C++ 14 compiler directive in your Makevars. 

`cat ~/.R/Makevars`

If you don't see a line like `CXX14=g++` or if you don't have a Makevars file then make sure you at least have a `.R` directory in `home` and then

`echo "CXX14 = g++ -std=c++1y -Wno-unused-variable -Wno-unused-function -fPIC" >> ~/.R/Makevars`
