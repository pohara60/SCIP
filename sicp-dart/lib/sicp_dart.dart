void run() {
  void f1() {
    print('f1');
  }

  void f2() {
    print('f2');
  }

  var table1 = make_table1();
  table1('insert')('f', '1', f1);
  table1('insert')('f', '2', f2);
  table1('display');
  table1('lookup')('f', '1')();
  table1('lookup')('f', '2')();

  var table2 = make_table2();
  table2('insert', ['f', '1', f1]);
  table2('insert', ['f', '2', f2]);
  table2('display');
  table2('lookup', ['f', '1'])();
  table2('lookup', ['f', '2'])();
}

Function make_table2() {
  var table = <String, Map>{};

  Function? lookup(key_1, key_2) {
    if (table.containsKey(key_1)) {
      if (table[key_1]!.containsKey(key_2)) {
        return table[key_1]![key_2];
      }
      return null;
    }
  }

  void insert(key_1, key_2, value) {
    if (!table.containsKey(key_1)) {
      table[key_1] = <String, Function>{};
    }
    table[key_1]![key_2] = value;
  }

  void display() {
    print(table);
  }

  dynamic dispatch(String m, [List? args = null]) {
    switch (m) {
      case 'lookup':
        if (args != null) {
          return lookup(args[0], args[1]);
        }
        break;
      case 'insert':
        if (args != null) {
          return insert(args[0], args[1], args[2]);
        }
        break;
      case 'display':
        return display();
    }
    return null;
  }

  return dispatch;
}

Function make_table1() {
  var table = <String, Map>{};

  Function? lookup(key_1, key_2) {
    if (table.containsKey(key_1)) {
      if (table[key_1]!.containsKey(key_2)) {
        return table[key_1]![key_2];
      }
      return null;
    }
  }

  void insert(key_1, key_2, value) {
    if (!table.containsKey(key_1)) {
      table[key_1] = <String, Function>{};
    }
    table[key_1]![key_2] = value;
  }

  void display() {
    print(table);
  }

  dynamic dispatch(m) {
    switch (m) {
      case 'lookup':
        return lookup;
      case 'insert':
        return insert;
      case 'display':
        return display();
    }
    return null;
  }

  return dispatch;
}
