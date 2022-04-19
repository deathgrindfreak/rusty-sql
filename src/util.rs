pub struct PrintTable {
    headers: Option<Vec<String>>,
    rows: Vec<Vec<String>>,
    sizes: Vec<usize>,
}

impl PrintTable {
    pub fn new() -> Self {
        PrintTable { headers: None, rows: Vec::new(), sizes: Vec::new() }
    }

    pub fn header(&mut self, row: &Vec<String>) {
        self.headers = Some(row.to_vec());
        self.update_sizes(&row);
    }

    pub fn add(&mut self, row: &Vec<String>) {
        if !self.rows.is_empty() && self.rows[0].len() != row.len() {
            panic!("Expected row of size {} received {}", self.rows[0].len(), row.len());
        }

        self.rows.push(row.to_vec());
        self.update_sizes(&row);
    }

    fn update_sizes(&mut self, row: &Vec<String>) {
        self.sizes = row.iter()
                        .enumerate()
                        .map(|(i, r)| r.len().max(*self.sizes.get(i).unwrap_or(&0)))
                        .collect();
    }

    pub fn print(&self) {
        if let Some(headers) = &self.headers {
            for (i, h) in headers.iter().enumerate() {
                print!(" {:width$}", h, width=self.sizes[i]);

                print!(" ");
                if  i != headers.len() - 1 {
                    print!("|");
                }
            }
            println!("");
        }

        for (i, s) in self.sizes.iter().enumerate() {
            print!("{}", "-".repeat(*s + 2));
            if  i != self.sizes.len() - 1 {
                print!("+");
            }
        }
        println!("");

        for row in &self.rows {
            for (i, cell) in row.iter().enumerate() {
                print!(" {:width$}", cell, width=self.sizes[i]);

                print!(" ");
                if  i != row.len() - 1 {
                    print!("|");
                }
            }
            println!("");
        }
    }
}
