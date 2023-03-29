---
title: "Delivrable WP IN construction "
subtitle: "Version: `r format(Sys.time(), '%B %d / %Y - %H : %M' )`"
#date: "Version Delivrable: "
#format: docx
format:
  pdf:
    toc: true
    #lot: true
    lof: true
    documentclass: article
    number-sections: true
    geometry:
      - top=30mm
      - left=20mm
    fontsize: 11pt
    colorlinks: true
    linkcolor: blue
    urlcolor: blue
    fig-pos: 'H'
    link-citations: yes
    include-in-header: [assets/packages.tex]
    include-before-body: [assets/titlepage.tex]
    keep-tex: false
    # include-in-header:
    #   - assets/packages.tex

execute:
  echo: false
  warning: false    
bibliography: [assets/References.bib]
csl: assets/journal-of-cleaner-production.csl   
subparagraph: yes
mainfont: Trebuchet MS
#pdf-engine: lualatex
prefer-html: true
---

[![hackmd-github-sync-badge](https://hackmd.io/FE8JCvJjS4CbjdMBf5DdGw/badge)](https://hackmd.io/FE8JCvJjS4CbjdMBf5DdGw)


```{r, echo=FALSE, include=FALSE}
library(tidyverse)
library(readxl)
library(knitr)
library(kableExtra)
```

\color{darkgray}

# Executive summary



# Introduction

This deliverable deals with the description and implementation test of the 3D printing of recycled plastic demonstrator. The ambition of this use case is to test the feasibility of the distributed recycling via additive manufacturing (DRAM) [@CruzSanchez2020] concept with the purpose to integrate in the Do-It-Together approach. There are two main goals: 

Evaluate the technical and logistical feasibility of the distributed recycling approach for the furniture sector, highlighting the advantages and barriers found in the implementation process. 

Provide a explain the a methodology in order  

The document is structured in three main parts: 

A baseline introduction is made regarding the plastic recycling issues in order to  

 

 

 

# Plastic Issues for the European Union

Since 1950’, our society have gained enormous advantages in terms of quality of life thanks to the technical development of the development of plastic and polymer materials. Plastic is a material that is widely used in our daily lives and plays a fundamental role in industry and economic development. The plastic material are found in almost all our products: food packaging, cars, technological tools, clothing, among others. The main reason is that plastic materials offer a variety of chemical and mechanical properties to be useful for a wide array of applications. Plastics are extremely useful, but their mismanagement has affected the environment and our health. The over-consumption and especially bad practices (single use, difficulty of reuse, etc.), make plastics one of the major societal challenges of an ecological transition that has become imperative. The main problem is the end-of-life treatment which traditionally uses a centralized system where plastic waste often has to travel thousands of kilometers... to be incinerated or landfilled. In addition to the energy and environmental impact of their production, there is also the impact of the end of life. 


Unfortunately, the plastic waste pollution poses a major threat because of the issue of non-degradability affecting the ecological environments [@Ryberg2019; @Hopewell2009; @Thompson2009b]. Indeed, recycling rates remain small (approx. 14%) in the plastic packaging field on a global scale [@Hahladakis2018]. Even in Europe, which tends to lead on environmental stewardship, the recycling rate is about 32.5 wt% [@Plastics2019]. However, these values consider the amount of plastic waste collected, rather than the total amount in circulation [@Kranzinger2018]. Rethinking the development and use of plastics is central to the circular economy paradigm, to provide less harmful options for the environment. Thus, more types of plastic packaging are available, but each reflects diverse circular economy strategies  

To tackle this accumulation waste problem, the European strategy for plastics in the circular economy (CE) is gaining attention in the policy and business debate surrounding sustainable development of industrial production [@EC2018; @Geissdoerfer2017]. 
CE tackles a central societal issue concerning the current principle "take, make, dispose" (linear economy) and its negative effects caused by the depletion of natural resources, waste generation, biodiversity loss, pollution (water, air, soil) and non-sustainable economics [@VanBuren2016]. The validation (technical, economic, legislative) of waste plastic as a secondary raw material in industrial processes is considered now a core target to integrate CE into the plastic value chain [@Simon2019]. 
Strategies of open and closed-loop recycling as well as upcycling and downcycling functionality approaches can offer paths to validate the secondary raw materials [@Zhuo2014].
The promotion of cross-sectorial valorization of plastic wastes through Industrial symbiosis approaches seems to be a relevant strategy for the circular economy strategies of the EU [@Karaylan2021] 

Based on this context, it is presented the demostration of the INEDIT project called '3D Printing of Recycling Plastic' that was developed and implemented. 
In the 
 

 

# Context of the 3D Printing of Recycled Plastic Demostrator 

## Presentation of the scale of the demostrator: Rives de Meurthe district (Nancy, France) 

The demonstrator is placed at the City of Nancy - France,  in the region of Lorraine at the  northeastern.
Nancy is the capital of the Meurthe-et-Moselle department and has a population of approximately 105,000 inhabitants. 
More precisely, our interest is the  Rives de Meurthe district as presented by the @fig-rives.
This district extends between the city center and the Meurthe River for about 7 km from north to south (extending into the municipalities of Jarville-la-Malgrange upstream and Maxéville downstream) and is between 250 and 1,000 m wide.

::: {#fig-rives layout="[35,-5,65]"}
![](figures/ok3/Rives-of-meurthe.png) 

![](figures/Image-to-add.png) 

Localization of the Rives the Meurthe distric
:::



Nancy was not born around a waterway and its commercial potential. 
Its port and river side has long been rather reduced, contrary to the great majority of cities. 
However, the main interest of the Rives de Meurthe  district concerns that it has been a case study in the light of urban regeneration due to flood risk presented in this area [@chiffre2014; @edelblutte2006]. 
Therefore, since end of 1980's, there have been a series of  renewal policies of the  district  with the purpose of going beyond a simple reconversion by broadly rethinking the role of the central and pericentral space ofo the city.


Among the multiples choices, one of the strategic actions taken by the government have been the transformation of the old site of the slaughterhouses in the heart of the Rives de Meurthe district.
In 1996, the slaughterhouse activity was transferred to the Épinal-Mirecourt ZAC, marking the end of the site's industrial life. 
As soon as the activities ceased, a rehabilitation process began in parallel with the development project of the district. 
The vast 6-hectare site was first carefully demolished to bring back the main buildings constructed at the beginning of the 20th century.

In 2017, the city administration took the decision by a public concertation to create exemplary actions in terms of of ecological transition at the city level [@villedenancy2018].
Thus, the creation on the site of the former slaughterhouses was taken.
This gives birth in 2019 to the creation of the OK3 association to develop and animate the cultural project of  *L'Octroi Nancy*
towards the creation of a Cultural and Creative Incubator.^[See more details in https://www.octroi-nancy.fr/]

Given the pandemic situation at the beggining on 2020, the end of works was only finished  in  2021. 


## Third place Octroi Nancy

The third place Octroi Nancy is a urban project transforms the former slaughterhouses of the city of Nancy into *"cultural, creative and citizen"* third place with 4600 $m^2$  of renovated buildings [@pallot2021].

![Overview of the Octroi Site](figures/ok3/OK3-00.jpg){#fig-ok3 width=90% }


Four large buildings (@fig-ok3) were refurbished to provide a convivial and multidisciplinary meeting place between culture and innovation; open to experimentation and intended to operate as a creative laboratory for the city.
The first building (1) are called the 'La Petite Halle' (*The Small Hall*) which is an space of 900 $m^2$. 
THe purpose is a creative laboratory from which projects of all artistic and creative disciplines can emerge.
The second building (2) is the 'L’Octroi Sud' (*South Octroi*) where it is intended the professionalization for the actors of the territory, through the installation of resource organizations.
The third building (3) is the 'La Grande Halle' (*The big Hall*).
It is a hangar building of 2,200 $m^2$ space for the organization of events, exhibitions and demonstration of artistic and cultural projects.
Finally, the fourth bilding (4) is the 'La Halle ouverte' (*the Open Hall*) which is an open space of 700 $m^2$ to host in particular a weekly organic market and several intermitent cultural activities mostly in the summer holidays.

![The Octroi Site](figures/ok3/OK3-01.jpg){#fig-ok1 width=90% }


In summary, these type of third places are open ecosystems that will bring together artists, researchers and creative people with the public, the city's inhabitants and businesses.
This can be considered as a socio-technical imaginary projects with new goals and development paths that depart from the existing that can give an an opportunity for socially inclusive and environmentally desirable urban transitions  in Europe [@Fratini2019] in terms of new roles of the local actors regarding the city development.

 
## Lorraine Fab Living Lab\textregistered

Connected to the Octroi ecosystem, the **Lorraine Smart Cities Living Lab (LSCLL)** is  a trans-disciplinary resource center of the Université de Lorraine.
It aims to support and link the different societal challenges of the Lorraine territory with the local resources.
It enables the integration of different users, implementing collaborative and agile approaches in the service of *Research, Development of Innovations, Training and a Citizen Culture*.
It experiments in terms of projects, governance and support platform since 2008,  involving several laboratories and other public and private partners as detailed in the @fig-lscll. 
Since 2010, this initiative is member of the European Network of Living Labs (ENoLL)^[4$^{th}$ wave of labelisation)], seeking to develop public-private-population partnerships (PPPPs) to disseminate innovation and related practices.

![Description of the Lo site](figures/lf2l/LSCLL.png){#fig-lscll width=65%} 


Since 2014, the LSCLL formalizes its strategic intention with the the Lorraine Fab Living Lab\textregistered (LF2L\textregistered) research platform for prospective assessment of innovative usages [@Dupont2016].  


![Photo of the LF2L](figures/lf2l/Methodology-complet-00.jpg){#fig-lscll width=65%} 



The LF2L physical environment is constituted by a collaborative and a fablab space.
The collaborative space allows users to foster co-operation in engineering design with different stakeholders in order to new create concepts/designs. 
On the other hand, the fablab space allows users to materialize the concepts/designs  in an easy and quick way in order to have an prospective evaluation [@Boujut2003; @Dupont2014; @Dupont2015b].
The synergy of these two spaces enables the project development in a living lab approach taking into account the user centered design principles. 

The conceptual framework is composed of three main elements as illustrated in @fig-lf2l-methodology: 

::: {layout-ncol=2}
![The Lorraine Fab Living Lab methodlogy.](figures/lf2l/Methodology-01.jpg){#fig-lf2l-methodology}

1. *Co-creation*: Creative process to find alternative resolution concepts to a problem-topic given integrating the key stakeholders in the process. 
2. *Prototyping*: Materialization (virtual/real) of the concept in order to have a first and quick in- sight. 
3. *Evaluation*: Establishment of the pertinence of the concepts in order to create a feedback/improvement process. 
:::


The conceptual innovation framework of LF2L takes into consideration the 2D (concept), 3D (object), 4D (over time) approaches involving different type of stakeholders (e.g. researches, companies, networks,) in order to have a foresight usage evaluation of a new concept, technology or project.
The stages and 2D/3D/4D resources allowing prospective assessment of innovative usages in order to support this conceptual framework inside this “innovation space” as indicated in figure 2.3 [@Dupont2015b; @Dupont2016]: 
This approach is useful to accelerate the deployment of industrial and/or urban demonstrators. 

<!-- Fabrication laboratories (“Fab Labs”) are a global network of local labs, enabling invention by providing access to tools for digital fabrication. In that sense, Fab labs are a particularly relevant form of the shared machine shop, specifying a minimal inventory of devices and machine tools necessary to implement the founding idea of “how to make (almost) everything” in any one lab [@Troxler2016; @Gershenfeld2005]. The first Fab Lab was installed in 2002 at the Massachusetts Institute of Technology (MIT); today there is a network of more than 2000 active workshops according to the fablab network (https://www.fablabs.io/labs/map) throughout the world.  In 2007, an international Fab Lab charter2 was adopted, establishing the essential principles of the Fab Lab form—e.g., public access to tools and knowledge.  -->

 


# 3D Printing of recycled plastic demonstrator: the “Green FabLab”
## Rationale for the technological system of the 3D printing  recycling demonstrator 

Based on the context local characteristics of the (LF2L\textregistered) and the Octroi ecosystems, the 3D printed demonstrator, also known locally as the *"Green Fablab"* is illustrated in the @fig-gf-2021. it is an initial socio-technical demonstrator of the distributed recycling approach that combines a living lab approach inside a citizen third place ecosystem.  

![Initial overview of the Green Fablab at November 2021](figures/2021-11-17-octroi.jpeg){#fig-gf-2021 width='90%'}

Initially, this demonstrator was inside the LF2L building that was a consolidation of previous research works [@sanchez2016]. 
Starting from November 2021 after the refurbishing works were finished, the Green Fablab was installed inside the Octroi ecosystem.

The Green FabLab demonstrator aims to experiment the technical feasibility and  evaluation of a distributed and local plastic recycling.
The main purpose is to valorize plastic waste for 3D printing technology and manual injection moulding techniques in the context of an open innovation space such as the Octroi Nancy. 
The results of this experimentation can be a baseline for many archetypes of open communities such as fablabs, hackerspaces or even industrial prototyping zones. 

The main purpose of this demonstrator in the INEDIT project is **to prove that plastic waste material can have several uses, and therefore several values, during its life cycle**. The same material could be recycled and transformed into new raw material for different products. It is in this spirit that many associations, SMEs, local authorities and individuals are developing new local recycling practices that could allow us to aim for an economy that is more respectful of the environment, fairer for society and more engaging for local politicians.

Therefore,  it is neccesary to understand the key conditions under which to deploy a notion of circular economy with plastic waste to possible establish a secondary raw material market for this asset.
Likewise, it is required the study of technical parameters for the technological diversity to possible use the waste material including  the open source 3D printers and manual desktop injection.
The outputs are, not only by minimizing use of the environment as a sink for residuals but – perhaps more importantly – by minimizing the use of virgin materials. 
Hence, the environmental impact of this technology is significantly reduced.

Moreover, taking into account the exponential growth of these spaces (third places, fablabs, Haker/makerspaces), they could help to increase the efficiency to the problem of polymer recycling through the development of a distributed recycling approach.
In these geographically distributed spaces, the polymer recycling process of the surrounding areas (streets, neighbourhood, industrial zones) will be carried out at small lot sizes minimizing, energy consumptions, and carbon emissions compared to the tradition centralized systems, as some researches have already explored this path.


## Distributed recycling via Additive Manufacturing DRAM

One major concept is the the Additive manufacturing (AM) -also known as 3D printing- technology which is an important industrial vector in the and its direct (and distributed) manufacturing capabilities.
This set of technologies are becoming a key industrial process that could play a relevant role in the transition from a linear to circular economy [@Despeisse2016]. 
AM technologies is expected to transform the production process [@Rahman2018; @Chen2017; @Jiang2017] thanks to its ability to transform a numerical model into a deposition of material (points, lines or areas) to create a 3D part [@Bourell2017]. 
The expiration of the first patents has contributed to an increased interest, creating consumer value and potential for disruption [@Beltagui2020; @West2016a]. In economic terms, the global additive manufacturing market is expected to reach USD 23.33 billion by 2026 [@ReportsAndData2019]. 
However, determining when and how to take advantage of the benefits is a challenge for traditional means of production. From a societal viewpoint, @Jiang2017 reported that the product development could change from traditional stage-gate models to iterative, agile processes changing the scenario by 2030. 

The technical development of INEDIT’s demonstrator is based on the **distributed recycling via additive manufacturing (DRAM) approach [@CruzSanchez2020]. This approach is a major scientific output from the INEDIT project as a proposition of the future industrial landscape**. 

DRAM is defined as the use of recycled materials by means of mechanical recycling process in the 3D printing process chain. 
In the literature, DRAM approach emphasizes the technical steps required to reuse plastic waste through the recycling chains for material-extrusion-based 3D printing [@CruzSanchez2020; @Little2020]. 
The use of recycled material, either in the form of raw material or blended with virgin material, is a method of special interest to contribute to sustainable manufacturing [@Zhao2018].  

@fig-dram illustrates the conceptual model of DRAM.  

![DRAM](figures/DRAM-10.png){#fig-dram width=90%}


In a general overview, the **Recovery (I)** phase concerns the logistic operations to consider to collect the plastic wastes to be reused in DRAM. 
The **Preparation (II)** phase corresponds to the actions and strategies to identify, separate, sort, size reduce and clean waste plastic to guarantee adequate quality for DRAM. 
The **Compounding (III)** phase refers to the development of mono- and composite-materials. 
The **Feedstock (IV)** phase identifies the actions to fabricate the material usable for the printing process, either filament for Fused Filament Fabrication (FFF) or the particle size for Fused Granular Fabrication (FGF). The **Printing (V)** stage identifies applications and process improvements for the recycled printed part. 
The **Quality (VI)** phase identifies the multi-level technical characterization performed to the recycled material.

In the DRAM methodology, consumers have an economic incentive to recycle. 
This is because they can use their waste as feedstock for a wide range of consumer products that can be produced for a fraction of the conventional cost of the equivalent products. 
Moreover, 3D printing is especially well suited because it enables the production of parts with (almost) no waste, and could reduce the waste related to the material by more than 40 %, reusing 95% of the unused material [@Petrovic2011].  
Currently, most of the cost of 3D printing is associated with filament [@Wittbrodt2013]. By recycling raw materials such as Polylactic acid (PLA), one of the most frequently used materials in 3D printing, it is possible to reduce the carbon dioxide emissions that are incurred by transport to landfills or shipping to customers, offering environmental benefits [@Santander2020]. 

A large number of products can already be manufactured with AM, which affects the geographical spread and density of global value chains [@Laplume2016].
It is expected that the reach of AM printable products will be much greater in the future, as the production of multi-material and built-in functionalities (e.g. electronics) will be possible to a large extent. 
In addition, the production of spare parts can be carried out on-site, modifying the role of suppliers in the production lines [@Zanoni2019]. 
@Matt2015 explored the stages of distributed model factories and decentralized production types ranging from distributed capabilities to cloud production. Thus, the need of transport will be much more carefully because the fact that AM will enable decentralization of production to localities near customers or in the most extreme distributed scenario at the customer's premises [@Petersen2017a; @BonninRoca2019; @Wittbrodt2013].
Moreover, AM technology makes it possible to reduce market entry barriers, reduce capital requirements and achieve an efficient minimum scale of production to promote distributed, flexible forms of production [@Despeisse2016].  

The distributed manufacturing/recycling approach enables an alternative option from an economy-of-scale to an economy-of-scope, where the products are highly personalized satisfying niche communities or even individuals [@Petrick2014; @Hienerth2014].
For these reasons, the AM technology could be a driver for a shift in manufacturing from globally distributed production to local facilities. 
Significant efforts are being made by industry and the scientific community to move AM techniques from rapid prototyping and tooling stages towards direct digital manufacturing (DDM) [@Mueller2012; @Holmstrom2016], with the concomitant environmental and social benefits. 
Nevertheless, @Niaki2019 demonstrated that environmental and social benefits are not the key preferential factors in the adoption of AM technologies in different industrial sectors. Only the economic factor remains relevant in the AM implementation, considering time- and cost-saving as the most important reasons. 

## Positionnement of Use case for OMDF Functions

Regarding the structuration of the INEDIT project[^D2.2],  the 3D printing of recycled plastic demonstrator is positioned in certain stages of the INEDIT approach as presented in the figure @fig-dit-ul.

![Conection of the DIT with the INEDIT approach](figures/DIT-UL.jpg){#fig-dit-ul width=90%}

On the co-creation phase, the use case deals with the prototyping aspect of the possible furniture.
On the other hand, in the open-manufacturing process, our use case deals mainly with the raw material sourcing, production and recycling aspect.
These outputs are also with a validation stage.

Additionally, in the light of the specification of the open manufacturing demonstration facilities (OMDF) framework[^D4.2] which defines the role and functions that the demonstrator need to assure at an industrial scale, 
@fig-omdf illustrated the connection of the of the primary, secondary and constraint functions of the OMDF with the 3D printing of recycled plastic demonstrator entails.

![Connection of the 'Green Fablab Use case with the Open Manufacturing Demonstration Facilities (OMDF) functions ](figures/Sankey-GF-Global.jpg){#fig-omdf width=95%}


As presented in the figure, several OMDF functions are treated in the this demonstrator with each stage of the distributed recycling approach.
A more detail analysis is made in the deliverable WP4 to explain the detailed success and missing criteria from the user case in the deployment phase. 

In the following lines, we explain the assumptions made in the deployment of the demonstrator and the technical characterization of each phase.
The technical characterization entails the technologies mobilized.



### Hypothesis of UL case for deployment in reality 

The implementation of the Green Fablab needs to be done considering certain assumptions and simplifications to reduce the complexity of this socio-technical system. 
The following assumptions were assumed in terms of geographical scale, material recollection and manufacturing aspects: 

- From a material perspective, only certain types of plastic wastes are considered. Specifically, Polyethylene terephthalate (PET), High density Polyethylene (HDPE), Polypropylene (PP) and Polylactic Acid (PLA). The major reason is from the technical perspective relies on the availability of these materials at the local area around the physical demonstrator. 

  - PLA is one of the most used plastics in 3D printing. Thus, as plastic waste source, PLA waste can be found from printed prototypes or 3D printed parts discarded.  
  - HDPE is a thermoplastic widely used in the packaging.  
  - PET is the main material of water bottles in the market. 

+ The sorting, separation and cleaning process of plastics wastes are critical processes of the recycling. Therefore, to possible make technical experimentation, the source waste niches needs to be with a non/low contaminaed level. For example, discarded 3D printing parts used for prototyping. They are usually mono-material and with a low level of impurities in the polymeric matrix.  

+ From a geographical point of view, only plastic waste collected from the smart collectors was considered. This is a as minimal viable option to possible control the input of material on the Green fablab facilities. 
 
<!-- The 3D printing activities carried out in these establishments have a specific purpose of making product prototypes and mock-ups, which allow to generate testing activities, design evaluations, functional evaluations, corrections. Therefore, after a short lifetime, 3D printing can be a source of significant amounts of plastic waste due to printed parts that do not possess the desired quality, unused raw materials, or products that have already fulfilled their life cycle [@Alexandre2020]  -->

Based on these assumption, we present the technical characterization of the Green Fablab 
 
## Technical characterization of the 3D printing of recycled demonstrator 

### Recovery I 

The first step in the implementation of the Green Fablab OMDF is the activity of *Recovery I*.
This phase aims to establish a minimal baseline logistic operations to consider to collect the plastic wastes to be recycled in the process. 
In the scientific literature, the reverse logistic and closed loop supply chains have been extensively studied in the scientific literature. 
For instance, @Santander2022 evaluated the benefits of a near loop and closed loop recycling network focused on additive manufacturing, mainly producing recycled filament. The main results show an economic and environmental benefit of sourcing filament from recycled plastic rather than purchasing exported virgin filament.  
This process is the first step to create a closed-loop supply network approach for the distributed manufacturing.  

<!-- In the scientific literature, the recovery is one of the main activities to considered in the recycling process given that this structures a supply chain that certainly is variable (i.e. in function of the public support to sort the material in the specific points of collection).  -->



The collection tasks consists of collecting plastic waste at different established points, which are then transported to a treatment center where it is recycled. 
The collection and recycling process aims to generate a recycling micro-network at the local level (neighborhood scale), which allows the recovery and revaluation of plastic waste through 3D printing. 
This allows to save impacts related to the traditional treatment of plastic waste, as well as to increase the recycling capacity in the city, giving more independence over the recycling process.
@fig-recovery illustrates the process model considered.

![Recovery processus of the Green Fablab](figures/Recovery.png){#fig-recovery}

The main difficult relies in the pertinent identification and  the quality state of the plastic waste.
Therefore, in the framework of the INEDIT project, the UL case demonstrator developed a “smart collector prototype” as illustrated in the @fig-smart-collector.
The complete documentation of the technical device can be found in the following open access reference [@gabriel2023].
Given the possible implementation in other contexts, the source files are shared in open-source repository with the purpose that open communities to take advantage the experiences developed at the Université de Lorraine. 
Eventually, the open communities can propose improvements and better versions. 

![Description of the developed Smart collector](figures/SC/Abstract.png){#fig-smart-collector width=400}

This is a relevant strategy given the cross-line of Industry 4.0 and circular economy, which is opening up fields such as smart waste management systems options to improve the effectiveness of different materials, including plastic waste [@Ranjbari2021] using information technology tools with the advent of the Internet of Things (IoT) [@fatimah2020, @rejeb2022].
Smart waste management system (SWMS) consists of public garbage collectors with embedded technology that is used to monitor real-time level of garbage bins in public places [@Bano2020]. 
The interest of this system is to optimize the path for the garbage collecting van that eventually reduces fuel cost. However, this work is mainly based on simulation. 
Therefore, there is an avenue to simplify experimentation in this domain using common open-source technology (hardware and software) [@Pearce2009] to implement projects that require heavy infrastructure such as routers and a gateway to deploy in the territory.  

The main functional requirement of the smart collector is to collect and provide data about plastic waste production in order to design a local and distributed recycling chain of value. However, the smart collector may be used in various use cases such as: 

- Monitoring the quantity of any other product that is collected over a large area.   
- Generating data about behavior to more precisely dimensions public infrastructure.  
- Monitoring the transformation and recycling process inside the transformation unit to follow the state and quantity of raw material and final product.  
- Initiating a digitization process in the waste management process as the information system element present here is flexible and commonly used in various types of projects. 

The device uses a controller compatible with batteries and use WAN technology to avoid the deployment of routers for data acquisition. Although using various types of sensors allows us to achieve better results [@Catania2014] by crossing data, the main indicator remains the weight.   

The process illustrated by the @fig-abstract  can be described in the as follows: 

1. **Smart Collector installation**: The first step is to identify the main actors in the neighborhood through meetings, visits and interviews in order to propose integration into the recycling network by installing a smart collector on their premises.  

1. **Supervision**: The monitoring is done through a dashboard that provides direct information sent by the smart collector. This allows to know the weight of each installed smart collector, allowing to have an approximation of its degree of occupancy. 

1. **Receiving and storing plastic waste**: The storage area must be organized and functional with respect to the needs of the demonstrator. 

1. **Plan and execute the collection**: This step aims to establish the collection routine.



The main result is to guaratee a constant supply chain of raw material that can be used inside the recycling facilities


### Preparation II  

The second phase of the corresponds to the actions and processes to identify, separate, sort, size reduce and clean waste plastic to guarantee with the purpose to obtain feedstock material that is adequate for the distributed recycling process. 
@fig-adequation displays an overview of the space and the machines used presented in the Green Fablab facilities to treat the plastic waste. 

![Adequation spaces for the preparation of the waste material](figures/preparation/adequation.jpg){#fig-adequation}


The plastic waste preparation process aims to conditioned the collected plastic to the requirements of 3D printing. For this process 3 main sub-processes are considered:  

- **Identification and Sorting**: These two processes aim to identify the type of plastic given the regular standard for the polymer industry.  The process of identification and separation of plastics is done manually and allows to separate the plastics that can be used as raw material for further production processes. 

- **Cleaning**: This process is aims to remove the traces of any other substance that may be present in the plastic waste. In this way the processing machines will not be exposed to possible anomalies linked to material impurities. 

- **Size reduction**: The size reduction process is carried out to possible obtain an adequate granulometry. This process allows to adapt the plastic waste for the direct injection process and/or the extrusion process. 

- **Drying phase**: This step prevents the formation of bubbles in the recycled material when it is melted during the following extrusion or densification step [@Niaounakis2013]. Moreover, complete elimination of water prevent hydrolytic decomposition of the molecular chains during the melting or plasticization, so that the treated material has to be as dry as possible. 


### Compounding III  

The *Compounding* phase is related to the operation, strategies in the development of composite materials using recycled feedstock intended to be use in a printing process. 
There have been several literature reviews about the technical aspect of composite materials in the additive manufacturing context [@Singh2017; @Hofstatter2017; @Brenken2017; @Mohan2017]. 

<!-- Special attention has been paid on material extrusion technologies in the production of polymer/composite feedstocks as it is economical, environmentally advantageous and adaptable to flexible filament material [@Singh2017]. For example, @Mohan2017 presented a review on composite materials and process parameters optimisation for the fused deposition modelling (FDM) process for improving the mechanical properties (i.e. tensile strength, fatigue). @Brenken2017 reported a detailed summary of mechanical properties of printed parts for different composite material for fused filament fabrication. Five majors strategies are elucidated from the literature review such as:  -->

In the context of the Green Fablab demonstrator of INEDIT project, the focus is to study the 
1) mono-recycled material and 2) the virgin-recycled blend material. 
The development of recycling niches of mono-material where the additive manufacturing can be implemented is key to study. 
While several different studies in laboratory conditions have been made to show the technical feasibility of recycling including HDPE [@Anderson2017, @Kreiger2013, Baechler2013], Biomass-derived poly(ethylene-2,5-furandicarboxylate) (PEF) [@Kucherov2017], PLA [@CruzSanchez2017], Linear Low Density Polyethylene (LLDPE)/ low density polyethylene (LDPE), [@Hart2018], thermoplastic elastomer (TPE) [@Woern2017].
<!-- In fact, @Hart2018 demonstrated the reconstitution of residual polymeric packaging waste from Meals-Ready-to-Eat (MREs) generated by soldiers around the world into additively manufactured appliances.  -->
<!-- One conclusion of these studies is the positive technical use of recycled mono-material for additive manufacturing purposes.  -->
However, it has to be highlighted that one major assumption of these studies relies in that the material used is already sorted, cleaned and using a same type of discarded product. 

For INEDIT project, the interest is to take into account the inner variability that could be in the recovery process, concerning the type of material given the fact, while there are seven types of recycling symbols for each type of polymer, one major constraint in the current systems is that each manufacturing company have a patented use of the additive in the polymer matrix, in order to fulfill its initial function of the product. 
 

### Feedstock IV 

The Feedstock III phase refers to the processes in order to transform the plastic waste into usable material material for the fabrication stage. Two outputs are seen in this etape: 1) the filament feedstock and 2) the pellet feedstock. The use of filament or pellet material are in coherence with the machine process used in the fabrication (cf section 4.4.5). The figure XX present the modeling of the process: 


The filament and pellet production process makes it possible to produce the necessary raw material from plastic waste. 
The production of these intermediate products allows the use of different technologies related. 
Before using these products (filaments and pellets) it is necessary to carry out evaluation tests to assess the geometrical characteristics that are necessary in the printing process. 
<!-- In the prosumer domain, there have been a models to create open source versions of extruder in order to create plastic filament: Lyman Filament Extruder [@Lyman2014], the Filabot [@McN2012], Recyclebot [@Baechler2013], RepRap Recycle Add-on [@Braanker2010], Precious plastic [@Hakkens2016]. @Woern2018a published a waste plastic extruder capable of making commercial quality 3-D printing filament. The filament production is made using a semi-open source commercial desktop extruder. -->

@fig-feedstock and table present the technical characteristics of the material equipement  


![Extrusion machine to fabricate recycled filament feedstock](figures/feedstock/3devo-00.jpg){#fig-feedstock width=90%}


### Fabrication process – Technological mix to valorize the recycled material 

In this step, the major output is the valorisation of the plastic waste material using different two alternative paths:
1) desktop injection moulding process (small and medium sizes), and 2), 3D printing process (fused filament fabrication --FF- and fused granular fabrication --FGF-). 

As matter of the validation of the demonstrator at TRL 6 level, the ambition of the demonstrator in the INEDIT project is to experiment and prove a technological ecosystem mix that seeks to valorise in a distributed approach different plastics for different purposes and stakeholders. 
Therefore, the initial choice is these two paths to create objects injected and 3D printed parts that are useful to the local ecosystem of the demonstrator. 
The technologies are presented in the following paragraphs.  


![Technological choices to recycle](figures/printing/technologies.jpg){#fig-printing width=90%}

 

#### Desktop injection moulding  

Injection moulding is one of the most used technique to form plastic materials.  
@fig-injection present the major technolgoies in the ‘Green Fablab’ case to propose a manual recycled aspect to possible reuse the plastic waste into small and medium plastics sheets.

![Manual injection in small and medium sizes](figures/printing/injectin-00.jpg){#fig-injection width=90%}



#### 3D printing process: Fused Filament & Granular Fabrication (FFF & FGF) 

In the era of the additive manufacturing technology, without a doubt, the material extrusion-based systems such as the fused filament fabrication (FFF) has been one of the prominent processes.
In fact, the technological development of open-source 3D printers is creating more affordable Additive Manufacturing (AM) machines for society in different applications. It provides the possibility of mass diffusion of this technology, and consequently, AM is being recognised as a disruptive that could up-end the last two centuries of approaches to design and manufacturing [@Pearce2014d, @Birtchnell2013a]. 

<!-- Several studies in the literature validate the interest of this technology from the materials, process and applications perspectives.  Different studies show that these technologies given the low energy level for manufacturing, represented the technologies the most used in the industry for prototyping, functional prototyping and eventually final part fabrication.   -->

In the Green Fablab demonstrator, we have two types of material-based systems: 1) Fused filament fabrication (FFF) and 2) Fused Granular Fabrication (FGF): 

<!-- FFF  -->
The principle of the filament fabrication was developed and patented in 1989 by Scott Crump as *Fused Deposition Modelling*, and since 2009, the technology became open source [@Crump1992], known as Fused Filament Fabrication, to establish the difference between the registred mark.
A schematic representation of this technology is presented in @fig-fff. 
This process usually uses thermoplastic polymer filaments that are heated until a temperature slightly higher than the melting temperature at the nozzle of the machine, reaching a semi-liquid state. 
At this point, the polymer is extruded on the platform to create the first layer of the object and after that, the polymer continues to be printed on top of the previous layer, so that, filament fuses with the previous layer and then is solidified at room temperature after printing [@Ngo2018, @CruzSanchez2017]. 

::: {#fig-fff layout-ncol=2}
![Fused filament fabrication -FFF- principle](figures/FFF-00.png){#fig-fff width=200}

![3D printinter machines](figures/printing/FFF.jpg){#fig-fff width=100}

Fused filament fabrication systems
:::


<!-- FGF  -->
On the other hand, the Fused Granular Fabrication is a direct extrusion systems of pellets is a key technical advancement to facilitate the use of recycled material in the printing process.
A schematic representation of this technology is presented in @fig-fgf. 
@Volpato2015 describes the development of a piston driven extrusion head that can extrude polypropylene granules into a filament. The head was designed to minimize the volume of material fused during the extrusion process and reduce the effect of material degradation.
@Canessa2017 developed a mini extruder for pellets or granules of recycled plastic that can be used in a RepRap FDM 3D printer for rapid prototyping. 
The use of Moineau pump technology to add precise volumetric control to the extrusion of pellets opens extraordinary new possibilities. It is important to mention that a Moineau pump has to be coupled to a first stage Auger screw. This ensures a continuous feed of melted plastic with-out inclusion of air bubbles, since the Moineau pump itself cannot guarantee such condition. However, it is also important to highlight that currently this technology is the phase of laboratory experimentation and initial market diffusion. The complexity  


::: {#fig-fgf layout-ncol=2}
![Fused granular fabrication -FGF- principle](figures/Canessa2017.png){#fig-hanno width=200}

![Fused filament fabrication -FFF- principle](figures/printing/Gigabot-05.jpg){#fig-fff width=200}


Additive manufacturing of material-extrusion based systems
:::


Gigabot X XL machine extruder has a long barrel with 3 heating elements or zone which helps in uniformly melting and viscosity control of the thermoplastic. 
T1 being the heating block near the nozzle while T2 being in the middle of T1 and T3. 
Gigabot X XL is equipped with nozzle of 1.75mm diameter which provides good deposition rate. 
As 3D printing smaller cross-section is very hard without a cooling system near the nozzle therefore a cooling system was designed, 3D printed using ABS material and installed onto the system


### Quality Assessment VI

The quality assessment phase is the process to assess the technical feasibility of the recycled part. 


# Operationalization of DIT process for the Use Case 
## Integration of the 3D Printing Recycled Plastic



Explanation of the INEDIT project but focusing on the Open Manufacturing Demonstration Facilities process 

```{r table}
Operationalization <- read_excel(path = "Tables/Tables.xlsx",
                                 sheet = "Operationalization",
                                 skip = 1)

Operationalization %>% 
   kbl(booktabs = T) %>% 
   kable_styling(latex_options = c("striped", "HOLD_position"), font_size = 10) %>% 
   column_spec(1, width = c("5cm")) %>% 
   column_spec(2, width = c("10cm")) %>% 
   column_spec(3, width = c("1.5cm"))
   
```
 



## Step 1 – Receive Design and Specification 


The first step in the reception of the design models and specifications from the INEDIT platform. 
The starting point of this activity is the downloading the respective documents that contains the 3D model to be manufactured by the use case as presented in the @fig-step1.

![Reception of the exploitable documents for the fabrication process](figures/Step-1.png){#fig-step1 width=80%}

One of the outputs of the co-creation phase of INEDIT plateform is the creation of a first initial model that can be exploitable in the open manufacturing process. 
In that way, the model is received taking into account the specific requirements of the customer, and the required inputs to determine if the technologies available in the demonstrated have the capacity to produce the product. 
In the case that it cannot be produced, it is necessary to notify immediately together with the arguments why it cannot be produced and offer ways of improvement. 


## Step 2 – Validation of the technical specifications of model to fabricate 

The main purpose of the second step is to establish the criteria for the  validation specifications of the model to fabricate.
In the case of the Green FabLab, three main criteria were established: 

![Validation of the printing conditions](figures/Step-2.png){#fig-step2 width=90%}

1. Concerning the dimmensions
2. Concerning the orientation and quality of the STL
3. Concerning the printability

Using the software SuperSlicer and the machine-specific configuration (e.g. for Gigabot or for ), it is validated that the global dimensions of the proposed part are coherent.
This needs to be in the range of the maximal working dimensions of the 3D printers. 

Another aspect, relies on the orientation of the part.



Lastly, the printability test are based on the characteristics of the material and the variables of the machine (namely, the temperatures of the barrel, the rotation of the stepper motors and the diamter of the nozzle).
Different tests of printability were made in order to have a baseline of usable printed part as illustrated in the @fig-printability.


:::{#fig-printability  layout-nrow=2 fig.pos='H'}
![Test of printability](figures/Swapnil-Results-01.jpg){#fig-printability1 width=500}

![Validation of the printability](figures/Swapnil-Results-02.jpg){#fig-printability2 width=500}

Experimental protocol to validate the printability tests.
:::


The test of printability consist in the selection the technical parameters of the machine (e.g. print speed, extrusion factor, temperature, layer heigth) using a Design of Experiments (DoE) approach. 
Then, with a basic benchmarking model (e.g. lines, cubes, pyramides in @fig-printability2), it is possible to identify the errors in the printing process using statistical approaoches as ANOVA and measures of standard error.

<!-- conclusion -->
A technical paper to describe in more detail the results of this printability approach is being prepared at the time of writing this final rapport.


## Step 3 – Identify local source of plastic waste 

This step seeks to establish a first network of plastic wastes source from the local ecosystem.
The task of the identification of local source of plastic is fundamental as the first stage in the recovery process.  
As illustrated in the @fig-fedoua-00, the methodological approach included the following steps: 

1. Pre-diagnosis of the territorial context, 
2. Identification of the actors, 
3. Analysis of the actors' involvement and implementation of the smart collector, and finally, 
4. Implementation & monitoring.

![Methodological steps for the identification of local sources of plastic wastes](figures/Fedoua-00.pdf){#fig-fedoua-00 width=90%}
 
<!-- 1) Pre-diagnosis of the territorial context,  -->
Regarding the pre-diagnosis phase, the purpose was to align the INEDIT project with the territorial context.
To do so, between XX-2021 and YY-2022,  the UL team had exchange with key actors of territorial development agencies and associations (Annex XX for more details) and the priorities in terms of sustainable development. 
The main output of this phase was the establishment of an initial Swot-Pestel analysis for the implementation of a local recycling network of plastic waste at the neighborhood of Rives de Meurthe at Nancy.



```{r table.SWOT, include=FALSE}
SWOT <- read_excel(path = "Tables/Tables.xlsx",
                                 sheet = "SWOT",
                                 skip = 0)

SWOT %>% select(-Reference) %>% 
   kbl(booktabs = T) %>% 
   kable_styling(latex_options = c("striped", "HOLD_position"), font_size = 9) %>% 
   column_spec(1, width = c("1.5cm")) %>% 
   column_spec(2, width = c("1.5cm")) %>% 
   column_spec(3, width = c("15cm")) %>% 
   landscape()
   
```



<!-- 2) Identification of the actors,  -->
Based on this first step, it was possible to identify relevant stakeholders in the local ecosystems to inquire on the issue of plastic wastes source. 
   <!-- Criteria 1 -->
First, they needeed to be in a  geographical range perimeter (less than 2km around the facilities) following the observations of [@CruzSanchez2020; @Santander2020].
Limiting the geographical perimeter of collection helps in the reduction of environmental impact because of the reduction of transport impact. 
   <!-- Criteria 1 -->
Second, the diversification of the actor profile that can be sensibilized to the participation of the collection (general public, employees,  students) and/or stakeholder's status (Public, Private, Associative) where the smart collector can be deployed. 
These two elements were essential to consider because the experimentation seeks to establish a baseline of the recovery process given the uncertainties of participation of the local context and the sensitization to the management of the plastic by the general public. 




::: {#fig-ecosystem layout-ncol=2}


![Local ecosystem interviewed about the implementation ofa 3D printing recycled demostrator](figures/fedoua/Ecosystem-01.pdf){#fig-ecosystem-00 width=100%}

![Type of stakeholders for the ](figures/fedoua/Ecosystem-00.pdf){#fig-ecosystem-01 width=100%}


LOcal ecosystem enquired
:::

<!-- We could obtain access to 23 relevant actors according to the following criteria.  -->
A total of 23 actors were interviewed in the period of time of XX-2020 - YY-2022, of which 21 by physical or telephone interview and 2 by electronic questionnaire
<!-- 23 out of the fifty actors solicited to participate in the survey agreed to participate ().  -->
They were mainly companies (X% small and Y% medium size), associative entities, academic sector. 
The diversity of the public was an interesting criterion for the study.
Participants in the economic, cultural and social dynamics of the district through their membership in the local association of economic actors of the territory.

The scope of activity of most of the respondents is local (at the level of the neighborhood or city) which may reflect a strong territorial anchoring and a commitment to local concerns and issues (waste management, social welfare, local job creation...). 
The majority of their business decisions are made locally, which reduces the risk of depending on the interests of entities outside the territory.


<!-- Acceptability  -->


::: {#fig-acceptability layout-ncol=2}

![Local ecosystem interviewed about the implementation ofa 3D printing recycled demostrator](figures/fedoua/Acceptability-01.pdf){#fig-acceptability-01 width=90%}

![Acceptability of the possible use of 'smart collector' for the ](figures/fedoua/Acceptability-02.pdf){#fig-acceptability-02 width=90%}

Answers of the local ecosystem enquired about the implementation of a through the smart collector prototype
:::


To do this, we identified the role of each actor, then the sources of plastic waste collection, and then identify the sources of 3D printing and potential synergies with LF2L. 
In order to achieve these results, an exchange with key actors in territorial development was necessary.  



<!-- Conclusion -->
Thanks to this approach, we have identified eigth collection sites at the local territory for the deployment of the smart collector.


## Step 4: Put in place smart collectors 

In this step the main purpose is the deployment of a set of *smart collectors* around the neighborhood.
@fig-sc-deployment presents the selected points around the Green FabLab for the installation of the prototype. 
The smart collector is produced and mounted manually. 
The specific details and step-by-step assemble process can be found in the technical paper [@gabriel2023].

![Deployment of the Smart Collectors.](figures/SC/Deployment.jpg){#fig-sc-deployment}


The selection of the places were based on the steps 3.
For the experimentation, 8 sites were selected for the deployment as listed in the @tbl-deployment.


```{r}
#| label: tbl-deployment
#| tbl-cap: "Selected points of deployment of the smart collector in the neighboorhood of Rives de Meurthe, Nancy - France."


Deployment <- read_excel(path = "Tables/Tables.xlsx",
                                 sheet = "Deployment")

Deployment %>% filter(Status == "OK") %>% select(-Status) %>% 
   kbl(booktabs = T) %>% 
   kable_styling(latex_options = c("striped", "HOLD_position"), font_size = 10) %>% 
   column_spec(2, width = c("3cm")) %>% 
   column_spec(4, width = c("2cm")) %>% 
   column_spec(5, width = c("4cm")) 
   # column_spec(2, width = c("10cm")) %>% 
   # column_spec(3, width = c("1.5cm"))

```


First,  face-to-face meetings with the local actors were made to obtain the agreement for the installation of the prototype. 
As a relevant criteria, the installation needed to be in a location were the visitors/employees/customers of the selected point are able to see the device. 
We designed an appropriate communication  that enables to explain the purpose of the device and connect to the information of INEDIT projet (see @fig-smart) 



::: {#fig-smart layout-ncol=2 fig.pos='H'}

![Picture of the Smart collector at the collection point](figures/SC/curves.jpeg){#fig-sc-curves width=2.5in}

![Communication strategy of the smart collector](figures/SC/comm.jpg){#fig-sc-flyer width=1.5in}

Smart collector
::: 

Then, a system activation is putted in place to begin the collection gate. 
Once the smart collector is online, it is necessary to survey the online dashboard to control the waste plastic quantity.
In the moment that the dashboard present a weight more than 3 kg, we mapped the collection point in the stage of *'to collect'* and we plan the recovery. 
The distance of the collection place is less than $2~km$ so is carried out by bicycle or on foot to avoid the possible impact produced for a combustion or electric vehicle. 
Once the recovery process is made, at the Green Fablab 
When the waste plastic is collected, it is stored at the facilities of the Green FabaLab before posterior treatment and adequation.  
we have build a central collector where the material is stored before it is treated. 

![Smart collectors deployed in the territory](figures/SC/Smart-Collectors.pdf){#fig-sc-flyer width=100%}



## Step 5: Transport waste material to the recycling facilities

The recovery process took place once a week on average. 
The plastic waste is collected and transported to Green Fablab facilities, and then it is stored in a central collector as illustrated by the figure @fig-sc-recovery.

::: {#fig-sc-collector layout="[[1,1], [1]]" fig.pos='H'}

![Centrla collector of plastic waste](figures/SC/Collector-bouchons-00.jpeg){#fig-sc-collector-00 width=2in}

![Communication flyer for the smart collector](figures/SC/Collector-bouchons-01.jpeg){#fig-sc-collector-01 width=2in}

Smart collector
::: 




Throughout the experimentation of the deployment, we have mapped the quantity of collected material. 
@fig-sc-recovery corresponds to the profile of quantity collection per month. 
In average, we have collected XX~kg per month.
 

![Recovery profile of plastic](figures/SC/Recovery.jpg){#fig-sc-recovery width=90%}


::: {.callout-note}
#### Key Performance Indicator of the Recovery process

In terms of *KPI* of the recovery, from February 2022 to February 2023, we have collected a total of $94.37~kg$ of plastic waste using 8 collectors in the territory of Rives de Meurthe, Nancy-France. 

:::





## Step 6: Adequation and preparation of the material 


This is the first stage carried out inside the Green FabLab.
This stage is the set of activities required for the plastic waste to be adapted for further use. 
The Green FabLab works mainly with 4 types of plastic at the moment. 
The most common are high density polyethylene (HDPE) and polypropylene (PP), which are the main plastics used in the production of bottle caps and they are collected in the smart collector. 
The plastic waste from unused/damaged 3D printing parts are mainly of polylactic acid (PLA) are collected from mainly from the Lorraine Fab Living Lab.
And finally, the plastic bottles also are collected which are polyethylene terephthalate (PET).


<!-- Sorting -->
The preparation process begins with the separation and identification of each plastic collected. As already mentioned, the plastics used in the Green FabLab are 4 (HDPE, PP, PET and PLA) and are separated by type of plastic and colour. This process is carried out manually. 


![Photo of the sorting process](figures/Image-to-add.png){width=50%}

 
<!-- Cleaning -->
The second step is the cleaning phase.
Cleaning and washing plastic cups and bottles is crucial step for effective recycling because plastics are mainly post-consumer waste, they are not in an adequate state of cleanliness. 
It is required that to ensure the plastic is as clean as possible because dirty material can affect the quality of the extrusion / printing process, which at the end affects the recycled product. 
Therefore, we aims to remove adhesives, leftover waste, and labels. 
HDPE and PP are mainly used in the plastic injection molding process, while PLA and PET (Mixed with 9% HDPE) are used in 3D printing.  


![Photo of the cleaning process](figures/Image-to-add.png){width=50%}


In a first moment, the manual cleaning is used in the Green Fablab to remove the most of the majors contaminant present in the material. 
For plastic injection moulding, where mainly PP and HDPE are used, the plastic is washed in a sink with hot water.  
The water consumption per gram is approximately $4L/1000g$. 
The drying of the plastic is done by natural convection in the open air. 


For additive manufacturing, where mainly PLA, HDPE and PET blends are used, the cleaning process is much more controlled. 
The process is carried out in a small ultrasonic cleaning machine, to ensure that impurities are removed. 
The cleaner ultrasonic machine wash 200gr de plástico en 1 L of water.
This process takes 20 mins with a consuption of 2kWh. 

![Photo of the Ultrasonic cleaning](figures/Image-to-add.png){width=500}


<!-- Size reduction -->
The second step in the preparation of the waste material is the size reduction process.
In this step, the washed and sorted plastic is sent through shredding machine where it is grounded into smaller pieces of plastic.
A critical parameters in the control of the granulometry. 
The purpose of the size reduction is to obtain plastic waste where the granulometry correspond to the extrusion / printing.
The plastic waste need to be in reduced from a range of between 25-50 mm to 3-5mm approximately after grinding.
A cutting mill machine SM 300 Retsch\textsuperscript{\textregistered}  with a selectable speed range from 700 to 3,000 $rpm$ was used.  The selected speed was $1500~rpm$.
Normally we use a rotational speed of 1500 which produces an energy consumption of 0.7 kWh. 
The process takes 15 minutes per kilogram of material with a loss of approximately 10%. 
For direct additive manufacturing the optimum size for the granulometry for printing is between 3 and 5mm.
Therefore, after shredding it is necessary to sieve.  
In terms of plastic injection moulding, the plastic flakes can be slightly larger than those required for 3D printing.   


![Photo of the Shredding process](figures/Image-to-add.png){width=500}



Filament is produced at 0.4 kg/h using 0.24 kWh/kg with a diameter ±4.6\%. 3Devo 

To remove all the moisture from the plastic it is necessary to carry out a drying process in a conventional oven.   
In the drying phase, the plastic is putted in the oven at 60°C during 15h with a consumption of 0,061 Kwh.  
 
<!-- Drying -->
Finally, the drying process is last step to prepare the material.


## Step 7: Path planning  - 3D Printing  

Once, we have the model to concerning the 


![Photo of the SuperSlicer ](figures/Image-to-add.png){width=500}


## Step 8 : Post-processing 


Post-processing relies to the treatment of the injected and/or printed part.
Regarding the injection part, the post-processing relies in the 


Concerning the 3D printing part, one of the most 

![Post-processing activities for the injection moulding and 3D printing processes](figures/post-processing.jpg)


## Step 9: Implementation Examples 

The different examples of implementation of the use case are presented in the following sections.
Each example aims to tackle step by step the complexity of the implementation of the DIT process at a TRL6 level.
Therefore, each example has a specific purpose.


###  Personalization of existing furniture

This first experimentation aimed to prove the design of a customized product.
Based on the printability tests, the initial model was developed using the CAD software Onshape to validate the technical printability of PLA virgin assets. 
Using the case of a personalization of a commercial furniture-arranging tool as displayed in @fig-demo1, several printed parts were manufactured to evaluate the technical pertinence of the reults as part a existing furniture.

![Personlizing a existing furniture](figures/Demo-01.jpg){#fig-demo1 width=80%}

In this case, only 3D printer Gigabot was used to validate the robutsness and the quality of the printed part.


###  Refurbishing of the an old furniture

In this case, the experimentation was a step further.
The main idea was to refurbishing of the an old wood workbench, connecting the tools of INEDIT.
Therefore, the idea was to use the scanner and the sketch features of the DesignTogether tool developed by the colleges of ENSAM / TTPS.
Based on that inputs, the manufacturing tools at the Green fablab including the 3D printing were mobilized.

First, once the workbench was dismantled, it was scanned using the an Ipad Pro considering  the technical characteristics needed for the application.
Then, the model was upload in the DesignTogether application in order to make a brainstorming ideas of features that are required to consider for the refurbishing. 
This was in input in the co-creation aspect of the process

:::{#fig-demo2 layout="[[35,-5,60], [1]]" fig.pos='H'}
![Initial recovered workbench](figures/demos/workbench/Initial.jpg){#fig-demo2.1 }

![Refurbished workbench](figures/demos/workbench/Final.jpeg){#fig-demo2.2}


![Refurbishing an old wood workbench using the INEDIT technologies](figures/demos/workbench/2022-02-24 Processus INEDIT.png){#fig-demo2.3 width=500}

Experimentation on refurbishing an wood workbench model
:::

Afterwards, the model enables a first materialization of the of the proposition that could be made. So, the different manual task started in function

\newpage
### Connecting the Recycling part and the Smartification

In this third experimentation, the idea was to connect the smartification process developed by the Uninova partners with our capabilities of manufacturing.
Therefore, as a part of the ICE-IAMOT conference demonstrator that took place on June 2022 at Nancy, 
we have built the structure of a kitchen furniture as presented in the @fig-fig-uninova. 


::: {#fig-uninova layout="[25,-5,70]" fig.pos='H'}
![Smartification of a kitchen](figures/demos/Uninova/uninova-00.jpg){#fig-uninova-00 }

![Smartification function in a recycled part](figures/demos/Uninova/uninova.jpg){#fig-uninova-01 }

Experimentation of smartification and 3D printing recycled use cases.
:::

The purpose was to built this piece of furniture to test the integration of the plastic and smartification technologies.
In this case, a recycled plastic bar was specifically made to be part of the entire furniture. 
There, it enables the sensor protection and masking of the sensor needed in the electrical mounting. 
Moreover, the value of the recycled material added a personalization finishing of the

\newpage
### Collaborative Desk building 

At the consortium, it was decided to build a collaborative desk.
The challenge in this experimentation was to connect all the different competences that are present in the different use cases.
Regarding our use case, we supported the creation of the prototype of this desk in a reduced scale using recycled filament.
Additionally, it was also the opportunity to make recycled production from printing and injection processes for the customization pieces.

Firstly, @fig-desk-00 illustrates several attempts made using the DesignTogether tool for ideas of personalization of the furniture. 
A workshop with 20 students of the National National School in Industrial Systems Engineering (ENSGSI) was organized to create several ideas on the same object.

![Co-creation stage on the personalization for the ](figures/demos/desk/desk-00.jpg){#fig-desk-00 width=95%}


Once the ideation phase was made, a second step was focused on the manufacturing of a small prototype of the desk using plastic assets as presented in figure @fig-desk-01.
This made possible to define the components that were manufacturing at real scale.

![Prototype of the desk](figures/demos/desk/desk-01.jpg){#fig-desk-01 width=95%}


The prototype enabled to identify three main customization object, namely a 1) PC monitor support, 2) an ajustable folder separation and the drawer handler. 
The PC monitor support was built entirely using the manual injection molding.
The drawer handler was completly 3D printed.
On the other hand, the ajustable folder was a combination of injection and 3D printed processes

::: {#fig-desk-proto layout-nrow=2}
![3D model of the recycled pieces to be made ](figures/demos/desk/desk-02.jpg){#fig-uninova-00 }

![Manufacturing of the recycled parts (PC support, adjustable folder separation and drawer handler)](figures/demos/desk/desk-03.jpg){#fig-uninova-01 }

Experimentation of the desk with the complete use cases of INEDIT.
:::


This experimentation was then confronted with the consortium to obtain a feedback about the possible improvements in the echnicall level.
But more importantly, to identify the possible continuum and interaction between the different technologies and models.
Figure

:::{#fig-desk-final layout="[60,-5, 35]"}
![Final assembling of the desk ](figures/demos/desk/desk-04.jpg){#fig-desk-04 }

![Exchange and discussion on the interaction and possible improvements](figures/demos/desk/desk-05.jpg){#fig-desk-05 }

Feedbacks on the 
:::


\newpage
### Bookshelf

Finnally


\newpage
### Local collaboration with the Green fablab: the case of the 'L'appaillet'

One important element of INEDIT project is the interaction with external designers and local ecosystem.
The implementation of the Green Fablab inside a citizen third place make this interaction valuable and fruitful to better align the expectations of designer and architect, with the possible maturity that the different technologies can have inside the INEDIT project.

The integration of the Green Fablab with the ecosystem of Octroi is a fruitful exchange of knowledge and interaction of know-how.
For intance, we have the possibility of the experimentation with the local association of designers called **L’A.Paillette** reagarding the design and build 3 mobile and movable modules to establish a kitchen corner for the association. 
In that case, the production consisted on 3 sheet in recycled plastic (400g per sheet), 96 plastic pin joints (20g per pin), having a total recycled plastic used about 3,1 kg. aprox.(around 800 bottle taps)

The initial model proposed by the association is presented on the left of the  @fig-apa-00.
Several iterations were need in order to transfor the initial requirement into possible manufactured pieces given the possiblitities of the technology presented of our use case.


![Final assembling of the desk ](figures/demos/apa/apa-00.jpg){#fig-apa-00 width=90%}


\newpage
### Future collaborationgs Liam






<!-- ## Step 10: Re-design and improvements of fabrication  -->

<!-- ## Step 11: Validation  -->



\newpage

# Conclusions

 

Plastic waste as secondary ressources and possibilities for a secondary market 

Qualification of the waste

Mains challenges to tackle 

Legislation of waste and  


More examples neeed it in the  recycling for education purposes 

 















































[^D2.2]: Delivrable 2.2 DIT DESIGN OF THE DIT APPROACH
AND XD FRAMEWORK
[^D4.2]: Delivrable 4.2 SPECIFICATION OF EACH PHYSICAL DEMONSTRATOR (OPEN MANUFACTURING


\newpage
# Bibliography