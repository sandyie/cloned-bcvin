README

Started 5/12/2020 by Phoebe Autio

<<<< Notes on bcvin brix cleaning scripts >>>>

 #'removed' notes unique columns that have been removed for cleaning purposes, but existed in the original table

-> 2004 : formatted, col "sampler" (removed)
-> 2005 : formatted, col "dMACH" (removed)
-> 2006 : formatted, col "sampler" (removed)
-> 2007 : formatted, col "sampler" (removed)
-> 2008 : formatted, cols tag.no, sampler (removed)
		#year, month, day are three columns created from the column “crush.tag.date”
-> 2009 : formatted, col "sampler" (removed)
-> 2010 : formatted, col "sampler" (removed)
-> 2011 : formatted, cols tag.no, appell, unalloc, tanks, deputy, location, bins, gcexclude (all removed)
		#year, month, day are three columns created from the column “crush.tag.date”
		#notes column was previously "comments". Value may be the individual who commented
-> 2012 : formatted, cols tag.no, appell, grapecost, tanks, deputy, bins (all removed)
		#year, month, day are three columns created from the column “crush.tag.date”
		#notes column was previously "comments". Value may be the individual who commented
-> 2013 : formatted
-> 2014 : formatted, cols grower, tag.no (removed)
-> 2015 : formatted, cols tag.no, appell, tanks, deputy, jtemp, location, bins (all removed)
-> 2017 : formatted
-> 2018 : formatted, cols time, Code, Owner, ID, region, product, malic, alpha.amino, ammonia, calculated, potassium, X (all removed)
    #empty rows removed
