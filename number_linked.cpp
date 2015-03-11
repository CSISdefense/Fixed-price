#include <set>
#include <map>
#include <ctime>
#include <fstream>
#include <iostream>
#include <string>
#include <cstdlib>

using namespace std;

class Contract {
public:
  int id;
  string office;
  string portfolio;
  int variants;
  double start; // start time
  double end; // end time; today if still ongoing (ie NA)
  bool equipment;
  bool read(ifstream& fh) {
    string field;
    struct tm date;

    // read contractid
    if (! getline(fh, field, ',')) return false;
    id = atoi(field.c_str());

    // read office stripping the quotes
    getline(fh, field, ',');
    office = field.substr(1, field.length() - 2);

    // read portfolio
    getline(fh, field, ',');
    portfolio = field.substr(1, field.length() - 2);

    // read number of variants
    getline(fh, field, ',');
    variants = atoi(field.c_str());

    // read start date
    getline(fh, field, ',');
    field += " 00:00:00"; // add the required time to the date
    strptime(field.c_str(), "%Y-%m-%d %H:%M:%S", &date);
    start = mktime(&date);

    // read the end
    getline(fh, field, ',');
    if (field.compare("NA") != 0) {
      field += " 00:00:00"; // add the required time to the date
      strptime(field.c_str(), "%Y-%m-%d %H:%M:%S", &date);
      end = mktime(&date);
    } else {
      end = time(NULL); // current time
    }

    // read the identified equipment
    getline(fh, field, ',');
    if (field.compare("NA") == 0 || atoi(field.c_str()) == 0) {
      equipment = false;
    } else {
      equipment = true;
    }
    
    // read the last irrelevant field
    if (fh >> field) return true;
    return false;
  }
  bool overlap(Contract other) const {
    // return true if timeframes overlap
    if (end < other.start || other.end < start) {
      return false;
    } else {
      return true;
    }
  }
};

struct contract_comparator {
  bool operator() (const Contract& lhs, const Contract& rhs) {
    if (lhs.office == rhs.office) return lhs.portfolio < rhs.portfolio;
    return lhs.office < rhs.office;
  }
};

typedef multiset<Contract>::iterator It;

int main() {

  // read the filename and then the data from it
  string file;
  cin >> file;
  ifstream fh;
  fh.open(file.c_str());
  Contract c;
  multiset<Contract, contract_comparator> contracts;

  while (c.read(fh)) contracts.insert(c);
  fh.close();

  cerr << "Read " << contracts.size() << " contracts\n";

  // data structure to hold the numbers of linked contracts
  map<int,int> linked; // indexed by contract id

  while (contracts.size() > 0) {
    c = *contracts.begin();
    pair<It,It> ret = contracts.equal_range(c); // range of contracts
						// with the same
						// office and
						// portfolio
    // for all pairs in the range
    It i1, i2;
    for (i1 = ret.first; i1 != ret.second; i1++) {
      for (i2 = i1, i2++; i2 != ret.second; i2++) {
	if (i1->overlap(*i2) && ! (i1->equipment && i2->equipment)) {
	  linked[i1->id]++;
	  linked[i2->id]++;
	}
      }
    }

    contracts.erase(ret.first, ret.second);
  }

  // output the linked numbers
  map<int,int>::iterator mit;
  for (mit = linked.begin(); mit != linked.end(); mit++) {
    cout << mit->first << " " << mit->second << endl;
  }
}
