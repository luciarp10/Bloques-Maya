import scala.collection.immutable.List;
import scala.collection.immutable.Set;

import javax.swing.*;
import java.awt.*;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.Buffer;
import java.util.ArrayList;
import java.util.concurrent.Semaphore;

public class Game extends JDialog {
    // Elementos de JDialog
    private JLabel nivelLabel;
    private JLabel vidasLabel;
    private JLabel puntuacionLabel;
    private JPanel mainPanel;
    private JPanel board;
    private JLabel puntuacionTotLabel;
    private JLabel partidasLabel;
    private JLabel tiempoLabel;

    // Estructuras de datos de control y lógica de juego
    private List<List<Object>> tablero;
    private Set<Object> colores;
    private int puntuacion_acumuluda = 0;
    private final ArrayList<Thread> hilos;

    // Estados de juego
    private boolean terminado = false;
    private Semaphore semaforo_IA;
    private Semaphore semaforo_relleno;

    /**
     * Constructor del JDialog, inicializa los componentes, hilos y los atributos.
     * @param nivel String que indica el nivel de dificultad
     * @param tiempo String con el tiempo inicial de la partida
     * @param esHumano Booleano que indica si va a jugar el usuario o la IA
     */
    public Game(String nivel, String tiempo, boolean esHumano) {
        // Inicializamos los componentes de swing
        setContentPane(mainPanel);
        setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);

        // Creamos un Listener para terminar los hilos al cerrar la ventana
        this.addWindowListener(new WindowAdapter() {
            @Override public void windowClosing(WindowEvent e) {
                terminarHilos();
            }
        });

        semaforo_IA = new Semaphore(1);
        semaforo_relleno = new Semaphore(1);

        // Inicializamos los atributos en función de los parámetros
        nivelLabel.setText(nivel);

        int vidas = nivel_to_int(nivel)[3]; // Obtenemos las vidas en función del nivel
        vidasLabel.setText(String.valueOf(vidas));

        puntuacionLabel.setText(0 + "");
        puntuacionTotLabel.setText(0 + "");

        // Obtenemos el tiempo en función del parámetro y los asignamos a los labels
        String[] tiempo_split = tiempo.split(":");
        int minutos = Integer.parseInt(tiempo_split[0]);
        int segundos = Integer.parseInt(tiempo_split[1]);
        tiempoLabel.setText(String.format("%02d:%02d", minutos, segundos));

        // Generamos un tablero aleatorio en función del nivel
        generarTablero(nivel);

        // Creamos e inicializamos los hilos
        hilos = new ArrayList<>(); // Inicializamos la estructura de datos

        // Si va a jugar la IA, creamos un hilo para la IA
        if (!esHumano) {
            hilos.add(new Thread(this::juego_IA));
            hilos.get(0).start(); // Iniciamos el hilo
        }

        // En cualquier caso, creamos un hilo para el reloj
        hilos.add(new Thread(() -> {
            while (!terminado) { // Mientras el juego no haya terminado
                // Actualizamos el tiempo cada vez que pasa un segundo y comprobamos que no hemos perdido
                try {
                    Thread.sleep(1000);
                } catch (InterruptedException e) {
                    break;
                }
                actualizar_tiempo(-1); // Decrementamos el tiempo en 1 segundo
                comprobarDerrota(); // Comprobamos si hemos perdido
            }
        }));
        hilos.get(hilos.size()-1).start(); // Iniciamos el hilo

        Thread hilo_rellenador = new Thread(new Runnable() {
            @Override
            public void run() {
                while (!terminado) { // Mientras el juego no haya terminado
                    try{
                        semaforo_relleno.acquire();
                        tablero = main.rellenar_huecos(tablero, colores);
                        actualizar_tablero(); // Actualizamos el tablero
                    }catch (InterruptedException e){
                        break;
                    }finally {
                        semaforo_IA.release();
                    }

                    try {
                        Thread.sleep(3000);
                    } catch (InterruptedException e) {
                        break;
                    }
                }
            }
        });
        hilos.add(hilo_rellenador);
        hilo_rellenador.start();
    }



    /**
     * Transforma una cadena que representa un nivel en un array de enteros
     * @param nivel String del dominio {Fácil, Medio, Difícil}
     * @return {filas, columnas, nivel_int, vidas}
     */
    private int[] nivel_to_int(String nivel){
        int[] niveles = switch (nivel) {
            case "Facil" -> new int[]{9, 11, 1, 8};
            case "Medio" -> new int[]{12, 16, 2, 10};
            case "Dificil" -> new int[]{25, 15, 3, 15};
            default -> new int[4];
        };
        return niveles;
    }

    /**
     * Comprueba que el tiempo no es 0 y no se han vaciado las vidas. Si es así, termina el juego.
     */
    private void comprobarDerrota(){
        String titulo = "";
        // Cadena genérica al terminar el juego
        String msg = "La partida ha finalizado con una puntuación de " + puntuacionTotLabel.getText() + " puntos en " + partidasLabel.getText() + " partidas.";
        // Configuramos el título en función de la causa de la derrota
        if (Integer.parseInt(vidasLabel.getText()) < 1)
            titulo = "Has perdido todas tus vidas";
        else if (tiempoLabel.getText().equals("00:00"))
            titulo = "Se acabó el tiempo";

        // Si hay alguna causa de derrota, mostramos el mensaje y terminamos el juego
        if (!titulo.isEmpty() && !terminado) {
            // Cerrojo que impide que se intente cerrar la venta más de una vez
            terminado = true;

            JOptionPane.showMessageDialog(null, msg, titulo, JOptionPane.INFORMATION_MESSAGE);

            // Cerramos la ventana actual y volvemos a abrir el menú.
            this.dispose();
            // Creamos un nuevo menú
            Menu menu = new Menu();
            // Mostramos el menú
            menu.pack();
            menu.setLocationRelativeTo(null);
            menu.setVisible(true);
        }
    }

    /**
     * Finaliza forzadamente la ejecución de los hilos IA y reloj.
     */
    private void terminarHilos(){
        // Termina completamente los hilos almacenados en la estrucutra
        for (int i = 0; i < hilos.size(); i++) {
            hilos.get(i).stop();
        }
    }

    /**
     * Comprueba si el tablero se ha vaciado (condición de victoria). Vuelve a llenarlo y restablece la puntuación y
     * las vidas.
     */
    private void comprobarVictoria(){
        if (main.tablero_vacio(this.tablero)) {
            puntuacion_acumuluda += Integer.valueOf(puntuacionLabel.getText());
            puntuacionTotLabel.setText(Integer.parseInt(puntuacionTotLabel.getText()) + puntuacion_acumuluda + "");
            puntuacion_acumuluda = 0;
            puntuacionLabel.setText("0");

            partidasLabel.setText(Integer.parseInt(partidasLabel.getText()) + 1 + "");

            int vidas = nivel_to_int(nivelLabel.getText())[3];
            vidasLabel.setText(vidas + "");

            generarTablero(nivelLabel.getText());
        }
    }

    /**
     * Envía las coordenadas recibidas al motor de juego en scala. Actualiza los datos del tablero, puntuación y vidas.
     * @param fila Fila de la casilla pulsada en el tablero.
     * @param columna Columna de la casilla pulsada en el tablero.
     */
    private void pulsar_bloque(int fila, int columna) {
        // Creamos una lista de scala (compatibilidad)
        List<Object> coords = main.create_list(fila, columna);

        // Obtenemos el tablero resultante de pulsar la casilla recibida
        List<List<Object>> nuevo_tablero = main.pulsar_bloque(this.tablero, main.obtener_columna(coords, 0), main.obtener_columna(coords, 1));
        // Actualizamos la puntuación por medio de scala y asignamos el resultado en swing
        puntuacionLabel.setText(String.valueOf(puntuacion_acumuluda + main.actualizar_puntuacion(tablero, nuevo_tablero, Integer.valueOf(puntuacionLabel.getText()), coords)));
        // Obtenemos el tablero final por scala y actualizamos el tablero a mostrar en swing
        this.tablero = main.desplazar_bloques(nuevo_tablero);

        // Si la puntuación llega a 0, restamos una vida
        if (puntuacionLabel.getText().equals("0")) {
            vidasLabel.setText(Integer.valueOf(vidasLabel.getText()) - 1 + "");
        }

        comprobarDerrota();
        // Cambiamos el background de los botones en función del nuevo tablero
        actualizar_tablero();
        // Comprobamos si el tablero se ha vaciado
        comprobarVictoria();

        semaforo_relleno.release();
    }

    /**
     * Generamos un tablero con colores aleatorios en función del nivel.
     * @param nivel String con el nivel de dificultad.
     */
    private void generarTablero(String nivel) {
        // Obtenemos los datos asociados al nivel.
        int[] nivel_int = nivel_to_int(nivel);

        // Obtenemos el tamaño del tablero
        int filas = nivel_int[0];
        int columnas = nivel_int[1];

        // Borramos el tablero anterior de pantalla y reconfiguramos el Layout
        board.removeAll();
        board.setLayout(new GridLayout(filas, columnas));

        // Generamos un tablero nuevo en función del nivel como entero (SCALA)
        this.colores = main.colores_tablero(main.n_colores(nivel_int[2]));
        this.tablero = main.generar_tablero(nivel_int[2], colores);

        // Creamos tantos botones como casillas tiene el tablero (SWING)
        for (int i = 0; i < filas; i++) {
            for (int j = 0; j < columnas; j++) {
                // Creamos un botón nuevo
                JButton button = new JButton();
                // El botón tendrá el nombre button_i_j, donde i y j son las coordenadas del botón
                button.setName("button_" + i + "_" + j);
                // Averiguamos el color del botón con scala y asignamos el color correspondiente al background
                button.setBackground(get_color(main.obtener_posicion(tablero, i, j)));
                button.setBorder(BorderFactory.createLineBorder(Color.BLACK));

                // Añadimos un ActionListener al botón para detectar los clicks
                button.addActionListener(e -> {
                    // Si el botón no representa un hueco
                    if(button.getBackground() != Color.BLACK) {
                        // Cuando el botón sea pulsado, averigua sus coordenadas por su nombre
                        String[] posicion = button.getName().split("_");
                        int fila = Integer.parseInt(posicion[1]);
                        int columna = Integer.parseInt(posicion[2]);

                        // Pulsa el bloque correspondiente a su posición (SCALA)
                        pulsar_bloque(fila, columna);
                    }else{ // Si pulsamos un hueco, mostramos un aviso
                        JOptionPane.showMessageDialog(null, "No puedes pulsar un bloque vacío");
                    }
                });

                // Añadimos el botón al tablero y a la lista de botones
                board.add(button);
            }
        }
    }

    /**
     * Obtiene el color en función del estado del bloque en el tablero.
     * @param n Estado del bloque.
     * @return Objeto Color con el color correspondiente.
     */
    private Color get_color(int n) {
        Color color_dev = switch (n) {
            case 1 -> Color.BLUE;
            case 2 -> Color.RED;
            case 3 -> Color.ORANGE;
            case 4 -> Color.GREEN;
            case 5 -> Color.LIGHT_GRAY;
            case 6 -> Color.MAGENTA;
            case 7 -> Color.GRAY;
            case 8 -> Color.WHITE;
            default -> Color.BLACK; // Negro por defecto
        };
        return color_dev;
    }

    /**
     * Actualiza el color de los botones en swing
     */
    private void actualizar_tablero()
    {
        int filas = this.tablero.length();
        int columnas = tablero.head().length();

        // Recorremos todos el tablero
        for (int i = 0; i < filas; i++) {
            for (int j = 0; j < columnas; j++) {
                // Obtenemos los botones según su posición y le asignamos el color correspondiente (SCALA)
                JButton button = (JButton) board.getComponent(i * columnas + j);
                button.setBackground(get_color(main.obtener_posicion(tablero, i, j)));
                board.repaint();
            }
        }
    }

    /**
     * Bucle de juego de la IA, selecciona la mejor coordenada (SCALA), busca el botón correspondiente y lo pulsa. (SWING)
     */
    private void juego_IA() {
        int columnas = tablero.head().length();

        // Mientras no haya terminado la partida
        while (!this.terminado) {
            // Si el tablero está vacío, esperar a que se llene
            if (main.tablero_vacio(this.tablero)) {
                continue;
            }

            try {
                semaforo_IA.acquire();

                // Obtenemos la mejor coordenada (SCALA)
                List<Object> coords = main.mejor_coordenada(this.tablero);
                //List<Object> coords = main.obtener_coordenadas_bloque_IA(main.list_to_par(this.tablero));
                // Obtenemos el botón correspondiente a esa coordenada (SWING)
                JButton button = (JButton) board.getComponent((main.obtener_columna(coords, 0)) * columnas + main.obtener_columna(coords, 1));
                // Pulsamos el botón como haría el usuario
                button.doClick();

                // Tiempo de espera para una mejor visualización
                /*try {
                    Thread.sleep(1000);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }*/
            } catch (InterruptedException e) {
                e.printStackTrace();
            }finally {
                semaforo_relleno.release();
            }
        }
    }

    /**
     * Actualiza el contador de tiempo en función de un estado.
     * @param estado Dirección del tiempo, 0 = reset, 1 = incrementar, -1 = decrementar
     */
    private void actualizar_tiempo(int estado) {
        // Obtenemos el tiempo actual de la interfaz
        int segundos = Integer.parseInt(this.tiempoLabel.getText().split(":")[1]);
        int minutos = Integer.parseInt(this.tiempoLabel.getText().split(":")[0]);

        // Aplicamos la operación al tiempo actual
        switch (estado) {
            case 0: // Reset a 00:00
                segundos = 0;
                minutos = 0;
                break;
            case 1: // Incrementar 1 segundo
                segundos++;
                if (segundos == 60) {
                    segundos = 0;
                    minutos++;
                }
                break;
            case -1: // Decrementar 1 segundo
                segundos--;
                if (segundos == -1) {
                    segundos = 59;
                    minutos--;
                }
                break;
        }
        // Aplicamos el cambio en la interfaz
        this.tiempoLabel.setText(String.format("%02d:%02d", minutos, segundos));
    }
}
