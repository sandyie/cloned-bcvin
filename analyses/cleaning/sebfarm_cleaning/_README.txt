README

Started 5/12/2020 by Phoebe Autio (PA)
Last Updated 8/20/2020 by PA

-------Notes on bcvin brix cleaning scripts-----

 #'removed' notes unique columns that have been removed for cleaning purposes, but existed in the original .csv
 #in cleaning years 2004-2018, SebFarms uses a mix of 2 digit 3 digit codes. These code conversions can be found in bcvin/data/sebastianfarms/codes/varietyDecoderWorking.csv
 #These are important to note, as although all codes were cleaned as if they had a block# (unless noted in script), some codes correlate with the 3 digit code and could instead not have a block. An example is provided on the next line.
 #Example. Full code=GABCHD, Vineyard=GAB, Variety=CH (two digit), block=D. If they for some reason used the 3 digit variety code (CHD), there would be no block.

-> 2004 : cleaned. col "sampler" (removed)
-> 2005 : cleaned. col "dMACH" (removed)
-> 2006 : cleaned. col "sampler" (removed)
-> 2007 : cleaned. col "sampler" (removed)
-> 2008 : cleaned. cols tag.no, sampler (removed).
-> 2009 : cleaned. col "sampler" (removed)
-> 2010 : cleaned. col "sampler" (removed)
-> 2011 : cleaned. cols tag.no, appell, unalloc, tanks, deputy, location, bins, gcexclude (all removed)
		      notes column was previously "comments". Value may be the individual who commented
-> 2012 : cleaned. cols tag.no, appell, grapecost, tanks, deputy, bins (all removed)
		      year, month, day are three columns created from the column “crush.tag.date”
          notes column was previously "comments". Value may be the individual who commented
-> 2013 : cleaned.
-> 2014 : cleaned. cols grower, tag.no (removed)
-> 2015 : cleaned. cols tag.no, appell, tanks, deputy, jtemp, location, bins (all removed)
-> 2017 : cleaned.
-> 2018 : cleaned. cols time, Code, Owner, ID, region, product, malic, alpha.amino, ammonia, calculated, potassium, X (all removed)

For years 2008, 2011, 2012, the columns year, month, and day were created from the column "crush.tag.date". This was different from the others.
Year 2018 contained empty rows within the original CSV in which either the dates, events, and values were empty, or the identifying material was empty.
This may have been an issue with the original reading of the CSV and should be looked into. The current cleaning reflects these errors, and removes all empty and unidentifiable rows.
