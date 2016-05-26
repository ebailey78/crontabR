# crontabR

Schedule cronjobs from R

`crontabR` is a package that makes it easy to schedule tasks in R with crontab. You can access the functionality through a new RStudio addin as well.

I have a lot of recurring reports and data processing that I need on a daily, weekly, or monthly basis. We have a RedHat server that I've installed the open-source version of RStudio Server. I have `crontabR` installed and it runs well withing RStudio Server. I've not attempted to run the addin from within the desktop version of RStudio.  

I've written documentation for all the functions now and cleaned up the underlying code a lot. I'm currently using `crontabR` to automate about half a dozen tasks at work now and everything seems to be working well. You can use the functions to set tasks but I recommend using the RStudio addin that I've created to accompany the package. It makes working with the package much easier in my opinion.

`crontabR` is still experimental and I'm going to continue to add features and fix bugs as I find them/think of them. Please let me know if you use the package and what you think of it.
