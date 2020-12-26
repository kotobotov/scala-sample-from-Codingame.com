
/**
 * Created by Kotobotov.ru on 11.03.2019.
 */

  import java.util.Scanner;
  import java.util.Comparator;
  import java.util.LinkedList;
  import java.util.Collections;

  class Coord implements Comparable<Coord> {
    private int X, Y;
    private int clock; // only used for special sorting, see below

    // hexagonal neighbourhood, only returns neighbours on the board
    public LinkedList<Coord> neighbours(int width, int height) {
      LinkedList<Coord> result = new LinkedList<Coord>();
      int colOffset = this.X % 2; // even columns are raised

      // checking whether new coordinate would lie within boundaries, set clockwise marker
      if (this.Y-1>=0) { // up
        result.add(new Coord(this.X, this.Y-1, 0));
      }
      if ((this.X+1<width)&&(this.Y-1+colOffset>=0)) { // right up
        result.add(new Coord(this.X+1, this.Y-1+colOffset, 2));
      }
      if ((this.X+1<width)&&(this.Y+colOffset<height)) { // right down
        result.add(new Coord(this.X+1, this.Y+colOffset, 4));
      }
      if (this.Y+1<height) { // down
        result.add(new Coord(this.X, this.Y+1, 6));
      }
      if ((this.X-1>=0)&&(this.Y+colOffset<height)) { // left down
        result.add(new Coord(this.X-1, this.Y+colOffset, 8));
      }
      if ((this.X-1>=0)&&(this.Y-1+colOffset>=0)) { // left up
        result.add(new Coord(this.X-1, this.Y-1+colOffset, 10));
      }
      return result;
    } // neighbours()

    // standard gets and toString()
    public int  getX() { return this.X; }
    public int  getY() { return this.Y; }
    public String toString() { return String.format("(%3d,%3d)", this.X, this.Y); }

    // comparable interface, sort in clockwise order (ascending)
    // the trick is knowing that sorting is only called for a list of neighbours
    // therefore the compareTo() method is NOT universal and relies on prior initialization of this.clock!
    @Override public boolean equals(Object o) { return (this.hashCode()==o.hashCode()); }
    @Override public int hashCode() { return this.toString().hashCode(); }
    @Override public int compareTo(Coord that) { return (this.clock - that.clock); }

    // Constructor
    public Coord(int x, int y) {
      this.X = x;
      this.Y = y;
      this.clock = -1;
    }
    public Coord(int x, int y, int clock) { // used for sorting, see above
      this.X = x;
      this.Y = y;
      this.clock = clock;
    }
  } // class Coord

  // a single cell containing the two tribes, sortable by number of casualties descending order, then clockwise
  class Comb implements Comparator<Comb> {
    private Coord pos;
    private int mauve, phlox;
    private int casualtiesMauve, casualtiesPhlox;
    private boolean contested;

    // basic boring sets and gets
    public void setContested() { this.contested = ((this.mauve>0)&&(this.phlox>0)); }
    public boolean getContested() { return this.contested; }
    public Coord getPos() { return this.pos; }
    public void setMauve(int m) { this.mauve = m; }
    public void setPhlox(int p) { this.phlox = p; }
    public void addMauve(int m) { this.mauve += m; }
    public void addPhlox(int p) { this.phlox += p; }
    public void subMauve(int m) { this.mauve -= m; }
    public void subPhlox(int p) { this.phlox -= p; }
    public void setCasualtiesMauve(int m) { this.casualtiesMauve = m; }
    public void setCasualtiesPhlox(int p) { this.casualtiesPhlox = p; }
    public int  getCasualtiesMauve() { return this.casualtiesMauve; }
    public int  getCasualtiesPhlox() { return this.casualtiesPhlox; }
    public int  getMauve() { return this.mauve; }
    public int  getPhlox() { return this.phlox; }

    // handing down. returns list of coordinates adjacent to this
    public LinkedList<Coord> getNeighbours(int width, int height) { return this.pos.neighbours(width, height); }

    // comparable interface, natural order is by position, never actually used
    // will always be called by specific comparator for each tribe
    @Override public boolean equals(Object o) { return (this.hashCode()==o.hashCode()); }
    @Override public int compare(Comb o1, Comb o2) { return 0; }
    @Override public int hashCode() {
      String tmp = posToString() + beeToString();
      return tmp.hashCode();
    } // hashCode()

    // SORTING by REVERSE ORDER!!!
    public static class Comparators {
      public static Comparator<Comb> MAUVE = new Comparator<Comb>() {
        @Override public int compare(Comb o1, Comb o2) {
          int result = o2.getCasualtiesMauve() - o1.getCasualtiesMauve();
          if (result==0) {
            result = o2.getPos().compareTo(o1.getPos());
          }
          return result;
        }
      };
      public static Comparator<Comb> PHLOX = new Comparator<Comb>() {
        @Override public int compare(Comb o1, Comb o2) {
          int result = o2.getCasualtiesPhlox() - o1.getCasualtiesPhlox();
          if (result==0) {
            result = o2.getPos().compareTo(o1.getPos());
          }
          return result;
        }
      };
    }

    public Comb copy() { // returns an exact copy of itself
      Comb result = new Comb(this.pos);
      result.setMauve(this.mauve);
      result.setPhlox(this.phlox);
      result.setCasualtiesMauve(this.casualtiesMauve);
      result.setCasualtiesPhlox(this.casualtiesPhlox);
      result.setContested();
      return result;
    }

    // return string representation as two lines for prettier debug
    public String posToString() { return this.pos.toString(); }
    public String beeToString() { return String.format("[%3d:%3d]", this.mauve, this.phlox); }
    public String puzzleOutString() { return String.format("%d %d", this.mauve, this.phlox); }

    // Constructor
    public Comb(Coord pos) {
      this.pos    = pos;
      this.mauve  = 0;
      this.phlox  = 0;
      this.casualtiesMauve = 0;
      this.casualtiesPhlox = 0;
      this.setContested();
    }

  } // class Comb

  class World {
    private int width, height;
    private int tick;
    private Comb[][] comb;
    private boolean DEBUG;

    // copies and returns the whole grid
    private Comb[][] copy() {
      Comb[][] result = new Comb[this.width][this.height];
      for (int x=0; x<this.width; x++) {
        for (int y=0; y<this.height; y++) {
          result[x][y] = this.comb[x][y].copy();
        }
      }
      return result;
    } // copy()

    // advances time by one tick
    public void tick() {
      // create working copy
      Comb[][] nextTickGrid = this.copy();

      // step -1: set flag for contested cells
      for (int y=0; y<height; y++) {
        for (int x=0; x<width; x++) {
          this.comb[x][y].setContested();
        }
      }

      // step 0: calculate outward fluxes for each cell, reinforcements
      for (int y=0; y<height; y++) {
        for (int x=0; x<width; x++) {
          // only flux for any reasons if uncontested!
          if (!this.comb[x][y].getContested()) {
            // list of neighbours
            LinkedList<Coord> neighbours = this.comb[x][y].getNeighbours(this.width, this.height);

            // step 0: reinforcements
            // two lists for the reinforcements
            LinkedList<Comb> battleGroundMauve = new LinkedList<Comb>();
            LinkedList<Comb> battleGroundPhlox = new LinkedList<Comb>();

            // only address cells with prior battles
            for (Coord co : neighbours) {
              if (this.comb[co.getX()][co.getY()].getCasualtiesMauve()>0) {
                battleGroundMauve.add(this.comb[co.getX()][co.getY()].copy());
              }
              if (this.comb[co.getX()][co.getY()].getCasualtiesPhlox()>0) {
                battleGroundPhlox.add(this.comb[co.getX()][co.getY()].copy());
              }
            }
            // sort by highest casualties, then clockwise order
            Collections.sort(battleGroundMauve, Comb.Comparators.MAUVE);
            Collections.sort(battleGroundPhlox, Comb.Comparators.PHLOX);
            // reinforce Mauve, reinforcements flux to nextTickGrid!
            for (Comb cb : battleGroundMauve) {
              int flux = cb.getCasualtiesMauve(); if (flux>this.comb[x][y].getMauve()) {flux = this.comb[x][y].getMauve(); }
              nextTickGrid[cb.getPos().getX()][cb.getPos().getY()].addMauve(flux);
              this.comb[x][y].subMauve(flux);
              if (DEBUG) System.err.println("Reinforcing from (" + x + "," + y + ") to " + cb.posToString() + " with " + flux + " Mauves.");
            }
            // reinforce Phlox
            for (Comb cb : battleGroundPhlox) {
              int flux = cb.getCasualtiesPhlox(); if (flux>this.comb[x][y].getPhlox()) {flux = this.comb[x][y].getPhlox(); }
              nextTickGrid[cb.getPos().getX()][cb.getPos().getY()].addPhlox(flux);
              this.comb[x][y].subPhlox(flux);
              if (DEBUG) System.err.println("Reinforcing from (" + x + "," + y + ") to " + cb.posToString() + " with " + flux + " Phloxes.");
            }
          }
        }
      }

      // re- updating grid with reinforcements. only for cells updated above
      for (int y=0; y<height; y++) {
        for (int x=0; x<width; x++) {
          if (this.comb[x][y].getCasualtiesMauve()>0) {
            this.comb[x][y].setMauve(nextTickGrid[x][y].getMauve());
          }
          if (this.comb[x][y].getCasualtiesPhlox()>0) {
            this.comb[x][y].setPhlox(nextTickGrid[x][y].getPhlox());
          }
        }
      }

      // step 1: calculate outward fluxes for each cell
      for (int y=0; y<height; y++) {
        for (int x=0; x<width; x++) {
          // only flux for any reasons if uncontested!
          if (!this.comb[x][y].getContested()) {
            // list of neighbours
            LinkedList<Coord> neighbours = this.comb[x][y].getNeighbours(this.width, this.height);
            // start values
            int currentMauve = this.comb[x][y].getMauve();
            int currentPhlox = this.comb[x][y].getPhlox();

            // step 1: outward fluxes
            // calculate total bees per tribe
            int totalMauve = currentMauve;
            int totalPhlox = currentPhlox;
            int neighMauve = 0; // number of fluxed to neighbours
            int neighPhlox = 0; // number of fluxed to neighbours
            // run through each neighbour and calculate totals
            for (Coord co : neighbours) {
              // add up all neighbouring bees by tribe, starting with current cell
              int coMauve = this.comb[co.getX()][co.getY()].getMauve();
              int coPhlox = this.comb[co.getX()][co.getY()].getPhlox();
              // only outward flux considered
              if (coMauve<currentMauve) {
                totalMauve += coMauve;
                neighMauve++;
              }
              if (coPhlox<currentPhlox) {
                totalPhlox += coPhlox;
                neighPhlox++;
              }
            } // for (all neighbours)
            // equilibrium
            double equilibriumMauve = totalMauve / (neighMauve+1);
            double equilibriumPhlox = totalPhlox / (neighPhlox+1);
            // individual fluxes
            for (Coord co : neighbours) {
              int coMauve = this.comb[co.getX()][co.getY()].getMauve();
              int coPhlox = this.comb[co.getX()][co.getY()].getPhlox();
              int fluxMauve = (int) ((equilibriumMauve - (coMauve)) / 2);
              int fluxPhlox = (int) ((equilibriumPhlox - (coPhlox)) / 2);
              // update if appropriate to nextTickGrid
              if (fluxMauve>0) {
                nextTickGrid[co.getX()][co.getY()].addMauve(fluxMauve);
                nextTickGrid[x][y].subMauve(fluxMauve);
                if (DEBUG) System.err.println("Moving from (" + x + "," + y + ") to " + co.toString() + " with " + fluxMauve + " Mauves.");
              }
              if (fluxPhlox>0) {
                nextTickGrid[co.getX()][co.getY()].addPhlox(fluxPhlox);
                nextTickGrid[x][y].subPhlox(fluxPhlox);
                if (DEBUG) System.err.println("Moving from (" + x + "," + y + ") to " + co.toString() + " with " + fluxPhlox + " Phloxes.");
              }
            } // for (all neighbours)
          } // if uncontested
        }
      }
      // step 2: execute fighting in each cell
      for (int y=0; y<height; y++) {
        for (int x=0; x<width; x++) {
          // start values
          int currentMauve = nextTickGrid[x][y].getMauve();
          int currentPhlox = nextTickGrid[x][y].getPhlox();
          nextTickGrid[x][y].setContested();

          // resetting losses for step 0
          nextTickGrid[x][y].setCasualtiesMauve(0);
          nextTickGrid[x][y].setCasualtiesPhlox(0);

          // only fight if contested!
          if (nextTickGrid[x][y].getContested()) {
            // tricking into integer division, factor represents the casualties
            int dividendMauve = -1;
            int dividendPhlox = -1;
            int divisorMauve = -1;
            int divisorPhlox = -1;

            if (currentMauve>4*currentPhlox) { // more than four times
              dividendMauve = 1;
              divisorMauve = 4;
              dividendPhlox = 1;
              divisorPhlox = 1;
            } else if (4*currentMauve<currentPhlox) { // more than four times
              dividendMauve = 1;
              divisorMauve = 1;
              dividendPhlox = 1;
              divisorPhlox = 4;
            } else if (currentMauve>2*currentPhlox) { // more than two times
              dividendMauve = 1;
              divisorMauve = 4;
              dividendPhlox = 1;
              divisorPhlox = 2;
            } else if (2*currentMauve<currentPhlox) { // more than two times
              dividendMauve = 1;
              divisorMauve = 2;
              dividendPhlox = 1;
              divisorPhlox = 4;
            } else if (currentMauve>currentPhlox) { // simple advantage
              dividendMauve = 1;
              divisorMauve = 3;
              dividendPhlox = 2;
              divisorPhlox = 3;
            } else if (currentMauve<currentPhlox) { // simple advantage
              dividendMauve = 2;
              divisorMauve = 3;
              dividendPhlox = 1;
              divisorPhlox = 3;
            } else if (currentMauve==currentPhlox) { // tie
              dividendMauve = 1;
              divisorMauve = 2;
              dividendPhlox = 1;
              divisorPhlox = 2;
            }
            // calculating casualties (maximum 60)
            int lossesMauve = 60; if (lossesMauve>currentMauve) { lossesMauve = currentMauve; }
            int lossesPhlox = 60; if (lossesPhlox>currentPhlox) { lossesPhlox = currentPhlox; }
            lossesMauve = (lossesMauve * dividendMauve) / divisorMauve;
            lossesPhlox = (lossesPhlox * dividendPhlox) / divisorPhlox;
            // removing casualties
            nextTickGrid[x][y].subMauve(lossesMauve);
            nextTickGrid[x][y].subPhlox(lossesPhlox);
            // report casualties to next tick, for step 0
            nextTickGrid[x][y].setCasualtiesMauve(lossesMauve);
            nextTickGrid[x][y].setCasualtiesPhlox(lossesPhlox);
            if (DEBUG) System.err.println("Battling: (" + x + "," + y + ") [" + currentMauve + ":" + currentPhlox + "] casualties {" + lossesMauve + ":" + lossesPhlox + "}");
          } // contested
        }
      }

      // swap back comb/nextTickGrid
      comb = nextTickGrid;
      // update time
      tick++;
    } // tick()

    // simple setters
    public void setInitialMauve(int x, int y, int mauve) { this.comb[x][y].setMauve(mauve); }
    public void setInitialPhlox(int x, int y, int phlox) { this.comb[x][y].setPhlox(phlox); }

    // for debugging only, stderr
    public void debugOut() {
      String spacer = "         ";
      String line1  = "";
      String line2  = "";
      System.err.println("Tick: " + this.tick);
      for (int y=0; y<this.height; y++) {
        line1 = this.comb[0][y].posToString();
        line2 = this.comb[0][y].beeToString();
        for (int x=2; x<this.width; x+=2) {
          line1 = line1 + spacer + this.comb[x][y].posToString();
          line2 = line2 + spacer + this.comb[x][y].beeToString();
        }
        System.err.println(line1);
        System.err.println(line2);
        line1 = "";
        line2 = "" ;
        for (int x=1; x<this.width; x+=2) {
          line1 = line1 + spacer + this.comb[x][y].posToString();
          line2 = line2 + spacer + this.comb[x][y].beeToString();
        }
        System.err.println(line1);
        System.err.println(line2);
      }
      System.err.println("");
    } // debugOut()

    public String puzzleOutString(int x, int y) {
      return this.comb[x][y].puzzleOutString();
    }

    // Constructor
    public World(boolean debug, int width, int height) {
      this.DEBUG = debug;
      this.tick   = 0;
      this.width  = width;
      this.height = height;
      this.comb   = new Comb[this.width][this.height];
      for (int x=0; x<this.width; x++) {
        for (int y=0; y<this.height; y++) {
          Coord pos = new Coord(x, y);
          this.comb[x][y] = new Comb(pos);
        }
      }
    }
  } // class World

  class Solution {

    public static void main(String args[]) {

      boolean DEBUG = true;

      Scanner in = new Scanner(System.in);
      int width = in.nextInt();
      int height = in.nextInt();
      World myWorld = new World(DEBUG, width, height);

      int outX = in.nextInt();
      int outY = in.nextInt();
      int outTick = in.nextInt();

      int N = in.nextInt();
      for (int i = 0; i < N; i++) {
        int X = in.nextInt();
        int Y = in.nextInt();
        int mauve = in.nextInt();
        int phlox = in.nextInt();
        myWorld.setInitialMauve(X, Y, mauve);
        myWorld.setInitialPhlox(X, Y, phlox);

      }

      if (DEBUG) { myWorld.debugOut(); }
      for (int i=0; i<outTick; i++) {
        myWorld.tick();
        if (DEBUG) { myWorld.debugOut(); }
      }

      System.out.println(myWorld.puzzleOutString(outX,outY));
    }
  }

