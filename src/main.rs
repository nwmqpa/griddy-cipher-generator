use clap::Parser;
use lazy_static::lazy_static;
use std::{
    collections::{HashMap, HashSet},
    fmt::{Display, Write},
    slice::SliceIndex,
};

lazy_static! {
    static ref CHAR_LOOKUP_TABLE: HashMap<char, [Shape; 3]> = {
        vec![
            ('A', [Shape::V, Shape::H, Shape::H]),
            ('B', [Shape::H, Shape::V, Shape::P]),
            ('C', [Shape::V, Shape::P, Shape::V]),
            ('D', [Shape::H, Shape::P, Shape::H]),
            ('E', [Shape::H, Shape::H, Shape::H]),
            ('F', [Shape::P, Shape::V, Shape::H]),
            ('G', [Shape::H, Shape::P, Shape::P]),
            ('H', [Shape::V, Shape::V, Shape::H]),
            ('I', [Shape::H, Shape::H, Shape::V]),
            ('J', [Shape::V, Shape::P, Shape::P]),
            ('K', [Shape::P, Shape::H, Shape::P]),
            ('L', [Shape::H, Shape::H, Shape::P]),
            ('M', [Shape::V, Shape::V, Shape::P]),
            ('N', [Shape::H, Shape::V, Shape::V]),
            ('O', [Shape::H, Shape::V, Shape::H]),
            ('P', [Shape::P, Shape::H, Shape::V]),
            ('Q', [Shape::P, Shape::P, Shape::H]),
            ('R', [Shape::P, Shape::H, Shape::V]),
            ('S', [Shape::V, Shape::H, Shape::V]),
            ('T', [Shape::V, Shape::V, Shape::V]),
            ('U', [Shape::P, Shape::V, Shape::V]),
            ('V', [Shape::V, Shape::P, Shape::H]),
            ('W', [Shape::H, Shape::P, Shape::V]),
            ('X', [Shape::P, Shape::V, Shape::P]),
            ('Y', [Shape::V, Shape::H, Shape::P]),
            ('Z', [Shape::P, Shape::P, Shape::V]),
        ]
        .into_iter()
        .collect()
    };
    static ref LINES_LOOKUP_TABLE: HashMap<[Shape; 5], Vec<((isize, isize), Segment)>> = {
        use Shape::*;
        use Segment::*;

        let data = [
            // Empty
            ([N, N, N, N, N], vec![]),
            ([N, N, N, N, H], vec![]),
            ([N, N, N, V, N], vec![]),
            ([N, N, N, V, H], vec![]),
            ([N, N, H, N, N], vec![]),
            ([N, N, H, N, H], vec![]),
            ([N, N, H, V, N], vec![]),
            ([N, N, H, V, H], vec![]),
            ([N, V, N, N, N], vec![]),
            ([N, V, N, N, H], vec![]),
            ([N, V, N, V, N], vec![]),
            ([N, V, N, V, H], vec![]),
            ([N, V, H, N, N], vec![]),
            ([N, V, H, N, H], vec![]),
            ([N, V, H, V, N], vec![]),
            ([N, V, H, V, H], vec![]),

            // Horizontal only
            ([H, N, N, N, N], vec![]),
            ([H, N, N, N, H], vec![((-1, 0), Horizontal)]),
            ([H, N, N, V, N], vec![]),
            ([H, N, N, V, H], vec![((-1, 0), Horizontal)]),
            ([H, N, H, N, N], vec![((0, 0), Horizontal)]),
            ([H, N, H, N, H], vec![((0, 0), Horizontal), ((-1, 0), Horizontal)]),
            ([H, N, H, V, N], vec![((0, 0), Horizontal)]),
            ([H, N, H, V, H], vec![((0, 0), Horizontal), ((-1, 0), Horizontal)]),
            ([H, V, N, N, N], vec![]),
            ([H, V, N, N, H], vec![((-1, 0), Horizontal)]),
            ([H, V, N, V, N], vec![]),
            ([H, V, N, V, H], vec![((-1, 0), Horizontal)]),
            ([H, V, H, N, N], vec![((0, 0), Horizontal)]),
            ([H, V, H, N, H], vec![((0, 0), Horizontal), ((-1, 0), Horizontal)]),
            ([H, V, H, V, N], vec![((0, 0), Horizontal)]),
            ([H, V, H, V, H], vec![((0, 0), Horizontal), ((-1, 0), Horizontal)]),

            // Vertical only
            ([V, N, N, N, N], vec![]),
            ([V, N, N, N, H], vec![]),
            ([V, N, N, V, N], vec![((0, 0), Vertical)]),
            ([V, N, N, V, H], vec![((0, 0), Vertical)]),
            ([V, N, H, N, N], vec![]),
            ([V, N, H, N, H], vec![]),
            ([V, N, H, V, N], vec![((0, 0), Vertical)]),
            ([V, N, H, V, H], vec![((0, 0), Vertical)]),
            ([V, V, N, N, N], vec![((0, -1), Vertical)]),
            ([V, V, N, N, H], vec![((0, -1), Vertical)]),
            ([V, V, N, V, N], vec![((0, -1), Vertical), ((0, 0), Vertical)]),
            ([V, V, N, V, H], vec![((0, -1), Vertical), ((0, 0), Vertical)]),
            ([V, V, H, N, N], vec![((0, -1), Vertical)]),
            ([V, V, H, N, H], vec![((0, -1), Vertical)]),
            ([V, V, H, V, N], vec![((0, -1), Vertical), ((0, 0), Vertical)]),
            ([V, V, H, V, H], vec![((0, -1), Vertical), ((0, 0), Vertical)]),

            // Plus only
            ([P, N, N, N, N], vec![]),
            ([P, N, N, N, H], vec![((-1, 0), Horizontal)]),
            ([P, N, N, V, N], vec![((0, 0), Vertical)]),
            ([P, N, N, V, H], vec![((-1, 0), CurveWS)]),
            ([P, N, H, N, N], vec![((0, 0), Horizontal)]),
            ([P, N, H, N, H], vec![((0, 0), Horizontal), ((-1, 0), Horizontal)]),
            ([P, N, H, V, N], vec![((0, 1), CurveSE)]),
            (
                [P, N, H, V, H],
                vec![((0, 0), Horizontal), ((-1, 0), Horizontal), ((0, 0), Vertical)],
            ),
            ([P, V, N, N, N], vec![((0, -1), Vertical)]),
            ([P, V, N, N, H], vec![ ((0, -1), CurveNW)]),
            ([P, V, N, V, N], vec![((0, -1), Vertical), ((0, 0), Vertical)]),
            (
                [P, V, N, V, H],
                vec![((0, -1), Vertical), ((0, 0), Vertical), ((-1, 0), Horizontal)],
            ),
            ([P, V, H, N, N], vec![((0, -1), CurveNE)]),
            (
                [P, V, H, N, H],
                vec![((0, -1), Vertical), ((0, 0), Horizontal), ((-1, 0), Horizontal)],
            ),
            (
                [P, V, H, V, N],
                vec![((0, -1), Vertical), ((0, 0), Horizontal), ((0, 0), Vertical)],
            ),
            (
                [P, V, H, V, H],
                vec![((0, -1), Vertical), ((0, 0), Horizontal), ((0, 0), Vertical), ((-1, 0), Horizontal)],
            ),
        ];
        data.into_iter().collect()
    };
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
enum Segment {
    Vertical,
    Horizontal,
    CurveNE,
    CurveNW,
    CurveSE,
    CurveWS,
}

impl Segment {
    pub fn to_str(&self, size: f64, stroke: f64, ) -> String {
        match self {
            Segment::Vertical => format!(
                r#"<rect width="{1}" height="{0}" x="{2}" />"#,
                size / 2.0,
                stroke,
                -stroke / 2.
            ),
            Segment::Horizontal => format!(
                r#"<rect width="{0}" height="{1}" y="{2}" />"#,
                size / 2.0,
                stroke,
                -stroke / 2.
            ),
            Segment::CurveNE => {
                format!(
                    r#"<path d="M 0 0 Q 0 {0} {0} {0}" stroke="black" stroke-width="{1}" fill="none" />"#,
                    size / 2.0,
                    stroke
                )
            }
            Segment::CurveNW => {
                format!(
                    r#"<path d="M 0 0 Q 0 {0} -{0} {0}" stroke="black" stroke-width="{1}" fill="none" />"#,
                    size / 2.0,
                    stroke
                )
            }
            Segment::CurveSE => {
                format!(
                    r#"<path d="M 0 0 Q 0 -{0} {0} -{0}" stroke="black" stroke-width="{1}" fill="none" />"#,
                    size / 2.0,
                    stroke
                )
            }
            Segment::CurveWS => {
                format!(
                    r#"<path d="M 0 0 Q {0} 0 {0} {0}" stroke="black" stroke-width="{1}" fill="none" />"#,
                    size / 2.0,
                    stroke
                )
            }
        }
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
enum Shape {
    V,
    H,
    P,
    N,
}

#[derive(Parser, Debug)]
#[clap(about, version, author)]
struct Args {
    /// File containing the text to parse
    #[clap(short, long)]
    filename: Option<String>,

    /// Text to process
    #[clap(short, long)]
    text: Option<String>,

    /// Base size in pixel of every squares
    #[clap(long, default_value_t = 50.0)]
    size: f64,

    /// Base stroke in pixel of every squares
    #[clap(long, default_value_t = 3.0)]
    stroke: f64,
}

struct CheckerRow {
    pub points: Vec<Vec<Shape>>,
}

impl Display for CheckerRow {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let data = self
            .points
            .iter()
            .map(|p| format!("{}", p.len()))
            .collect::<Vec<_>>()
            .join(" ");
        f.write_str(&data)?;
        Ok(())
    }
}

struct Checker {
    pub dx: usize,
    pub dy: usize,

    pub rows: Vec<CheckerRow>,
}

impl Checker {
    pub fn new(letters: usize) -> Self {
        assert!(letters > 0, "Letters must not be 0");

        let dx = 6 + letters;
        let dy = 2 + letters;
        let rows = (0..dy)
            .map(|_| CheckerRow {
                points: (0..dx).map(|_| vec![]).collect(),
            })
            .collect();

        Self { dx, dy, rows }
    }

    pub fn add_to_x_y(&mut self, shape: Shape, x: usize, y: usize) -> Option<()> {
        self.rows.get_mut(y)?.points.get_mut(x)?.push(shape);
        Some(())
    }

    pub fn add_shape(
        &mut self,
        shape: Shape,
        shape_index: usize,
        letter_index: usize,
    ) -> Option<()> {
        let cx = 1 + (shape_index * 2) + letter_index;
        let cy = 1 + letter_index;

        match shape {
            Shape::V => {
                self.add_to_x_y(shape.clone(), cx, cy)?;
                self.add_to_x_y(shape.clone(), cx, cy - 1)?;
                self.add_to_x_y(shape, cx, cy + 1)?;
            }
            Shape::H => {
                self.add_to_x_y(shape.clone(), cx, cy)?;
                self.add_to_x_y(shape.clone(), cx - 1, cy)?;
                self.add_to_x_y(shape, cx + 1, cy)?;
            }
            Shape::P => {
                self.add_to_x_y(shape.clone(), cx, cy)?;
                self.add_to_x_y(shape.clone(), cx - 1, cy)?;
                self.add_to_x_y(shape.clone(), cx, cy - 1)?;
                self.add_to_x_y(shape.clone(), cx + 1, cy)?;
                self.add_to_x_y(shape, cx, cy + 1)?;
            }
            Shape::N => {}
        }

        Some(())
    }

    pub fn get_x_y(&self, x: isize, y: isize) -> Option<Vec<Shape>> {
        if x < 0 {
            return None;
        }

        if y < 0 {
            return None;
        }

        Some(self.rows.get(y as usize)?.points.get(x as usize)?.clone())
    }

    pub fn get_stride(&self, x: isize, y: isize) -> Option<[Shape; 5]> {
        let center = self.get_x_y(x, y).unwrap_or_else(|| vec![]);
        println!("{:?}", center);
        let center = if (center.contains(&Shape::V) && center.contains(&Shape::H))
            || center.contains(&Shape::P)
        {
            Shape::P
        } else {
            center.first().copied().unwrap_or_else(|| Shape::N)
        };

        let north = self.get_x_y(x, y - 1).unwrap_or_else(|| vec![]);

        let north = if north.contains(&Shape::V) || north.contains(&Shape::P) {
            Shape::V
        } else {
            Shape::N
        };

        let south = self.get_x_y(x, y + 1).unwrap_or_else(|| vec![]);
        let south = if south.contains(&Shape::V) || south.contains(&Shape::P) {
            Shape::V
        } else {
            Shape::N
        };

        let east = self.get_x_y(x + 1, y).unwrap_or_else(|| vec![]);
        let east = if east.contains(&Shape::H) || east.contains(&Shape::P) {
            Shape::H
        } else {
            Shape::N
        };

        let west = self.get_x_y(x - 1, y).unwrap_or_else(|| vec![]);
        let west = if west.contains(&Shape::H) || west.contains(&Shape::P) {
            Shape::H
        } else {
            Shape::N
        };

        Some([center, north, east, south, west])
    }

    pub fn generate_svg(&self, size: f64, stroke: f64) -> Option<String> {
        let mut groups: HashMap<(isize, isize), Vec<Segment>> = HashMap::new();

        for y in 0..self.dy {
            for x in 0..self.dx {
                let stride = self.get_stride(x as isize, y as isize)?;

                println!("({}, {}): {:?}", x, y, stride);

                let lines = LINES_LOOKUP_TABLE
                    .get(&stride)
                    .expect(&format!("{:?}", stride));

                if lines.is_empty() {
                    continue;
                }

                for ((dx, dy), s) in lines {
                    let entry = groups
                        .entry(((x as isize + dx), (y as isize + dy)))
                        .or_insert(vec![]);
                    entry.push(*s);
                }
            }
        }

        let mut svg_data = vec![];

        for ((x, y), segs) in &groups {
            let mut segs_str = vec![];

            for seg in segs {
                match seg {
                    Segment::Vertical => {
                        if segs.contains(&Segment::CurveNE) || segs.contains(&Segment::CurveNW) {
                            continue;
                        }
                        if let Some(segs) = groups.get(&(x - 1, *y)) {
                            if segs.contains(&Segment::CurveWS) || segs.contains(&Segment::CurveNW) {
                                continue;
                            }
                        }
                        if let Some(segs) = groups.get(&(*x, y + 1)) {
                            if segs.contains(&Segment::CurveSE) {
                                continue;
                            }
                        }
                    }
                    Segment::Horizontal => {
                        if segs.contains(&Segment::CurveWS) || segs.contains(&Segment::CurveSE) {
                            continue;
                        }
                        if let Some(segs) = groups.get(&(*x, y - 1)) {
                            if segs.contains(&Segment::CurveNE) || segs.contains(&Segment::CurveNW) {
                                continue;
                            }
                        }
                        if let Some(segs) = groups.get(&(*x + 1, y - 1)) {
                            if segs.contains(&Segment::CurveNW) {
                                continue;
                            }
                        }
                        if let Some(segs) = groups.get(&(*x, y + 1)) {
                            if segs.contains(&Segment::CurveSE) {
                                continue;
                            }
                        }
                    }
                    _ => {}
                }
                segs_str.push(seg.to_str(size, stroke));
            }

            svg_data.push(format!(
                r#"<g transform="translate({},{})">{}</g>"#,
                *x as f64 * size / 2.0,
                *y as f64 * size / 2.0,
                segs_str.join("\n")
            ))
        }

        Some(format!(
            r#"
<svg width="1000" height="1000">
    {}
</svg>w 
"#,
            svg_data.join("\n    ")
        ))
    }
}

impl Display for Checker {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for row in &self.rows {
            let str = format!("{}\n", row);
            f.write_str(&str)?;
        }
        Ok(())
    }
}

fn main() {
    let args = Args::parse();

    if !(args.filename.is_some() ^ args.text.is_some()) {
        panic!("Text XOR filename should be specified");
    }

    let text = if let Some(filename) = args.filename {
        std::fs::read_to_string(&filename).expect("Couldn not read file")
    } else {
        args.text.expect("Should be Some()")
    };

    let words = text
        .split(|c: char| !c.is_alphabetic())
        .filter(|s| !s.is_empty())
        .map(|s| s.to_uppercase())
        .collect::<Vec<_>>();

    let new_words = words
        .iter()
        .map(|s| {
            (
                s,
                s.chars()
                    .map(|c| CHAR_LOOKUP_TABLE.get(&c).cloned())
                    .chain([Some([Shape::N, Shape::N, Shape::N])].into_iter())
                    .collect::<Option<Vec<_>>>(),
            )
        })
        .filter_map(|s| s.1.and_then(|c| Some((s.0.clone(), c))))
        .collect::<Vec<(String, Vec<[Shape; 3]>)>>();

    for (word, comps) in new_words {
        let mut checker = Checker::new(word.len());

        for (li, shapes) in comps.iter().enumerate() {
            for (si, shape) in shapes.iter().enumerate() {
                checker.add_shape(*shape, si, li).unwrap();
            }
        }
        let svg = checker.generate_svg(args.size, args.stroke).unwrap();

        std::fs::write(format!("{}.svg", word), &svg);

        println!("{}", checker);
    }
}
