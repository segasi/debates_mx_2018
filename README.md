# Debates presidenciales en México en 2018

En este proyecto de RStudio encontrarás el código que Juan Ricardo Pérez y yo utilizamos para el análisis de texto de los tres debates presidenciales ([primero](https://www.nexos.com.mx/?p=37594), [segundo](https://www.nexos.com.mx/?p=37668) y [tercero](https://www.nexos.com.mx/?p=38026)) de México en 2018.

En los tres casos, la transcripción del debate fue llevada a cabo por [Eficiencia Informativa](https://data4.efinf.com/josso/signon/login.do?josso_back_to=https://data4.efinf.com/reader/josso_security_check).

<p align="center">
  <img src="http://segasi.com.mx/clases/cide/vis_man/datos/tenor.gif">
</p>

El proyecto incluye cuatro folders: 

- En el folder **01_datos** encontrarás las transcripciones en Word de cada uno de los debates. Sin embargo, para procesar estos archivos en el código los descargamos de Internet.

- En el folder **02_codigo** encontrarás el código del análisis de cada uno de los debates, así como un cuarto script en el que comparamos los datos de los tres. Cada uno de los scripts es independiente de los otros.

- En el folder **03_graficas** encontrarás las gráficas generadas para analizar cada debate y la comparación de los tres. En esta carpeta hay más gráficas de las que publicamos. 

- En el folder **04_datos_output** encontrarás tres archivos en formato .csv con la base de datos de los diálogos de cada debate por separado, así como un archivo que incluye todos los diálogos de los tres debates. Además encontrarñas las bases de datos de enlaces y nodos utilizadas para construir el diagrama de red de menciones de cada debate.

**Aclaración**: los resultados de estos scripts pueden variar respecto a los resultados que reportamos inicialmente en Nexos y Oraculus. Esto se debe a que cuando preparábamos el tercer análisis, así como este repositorio, nos dimos cuenta de algunos errores menores que corregimos. 

Desde nuestra perspectiva, la diferencia más significativa radica en que en el análisis del segundo debate dijimos que Yuriria Sierra había mencionado 2,606 palabras y León Krauze 2,340. Sin embargo, las cifras correctas son 2,514 y 2,601, respectivamente. Esto implica, primero, que Krauze dijo más palabras que Sierra, y, segundo, que ambos moderadores mencionaron más palabras que López Obrador.


