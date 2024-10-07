module Keyboard exposing (..)

import Array exposing (Array)
import Bitwise exposing (shiftLeftBy, shiftRightBy)
import Char exposing (toUpper)
import Dict
import String exposing (fromChar)
import Vector5 exposing (Vector5)
import Vector8 exposing (Vector8)
import Z80Debug exposing (debugLog, debugTodo)


type Kempston
    = JoystickLeft
    | JoystickRight
    | JoystickUp
    | JoystickDown
    | JoystickControl



--type ControlKey = Shift | Control | Enter | Alt | AltGraph | Backspace | Delete | Home | Escape |
--                  End | PageUp | PageDown | NumLock | ArrowDown | ArrowUp | ArrowLeft | ArrowRight |
--                  Insert | F1 | F2 | F3 | F4 | F5 | F6 | F7 | F8 | F9 | F10 | F11 | F12


type ControlKey
    = Shift
    | Control
    | Enter
    | Alt
    | Backspace
    | Escape
    | ArrowLeft
    | ArrowDown
    | ArrowUp
    | ArrowRight



--| Delete | Home  |
--                  End | PageUp | PageDown | NumLock | ArrowDown | ArrowUp | ArrowLeft | ArrowRight |
--                  Insert | F1 | F2 | F3 | F4 | F5 | F6 | F7 | F8 | F9 | F10 | F11 | F12
-- actual supported control keys


c_CONTROL_KEY_MAP =
    Dict.fromList
        [ ( "Shift", Shift )
        , ( "Control", Control )
        , ( "Enter", Enter )
        , ( "Escape", Escape )
        , ( "Alt", Alt )
        , ( "Backspace", Backspace )
        , ( "ArrowLeft", ArrowLeft )
        , ( "ArrowDown", ArrowDown )
        , ( "ArrowUp", ArrowUp )
        , ( "ArrowRight", ArrowRight )
        ]


type alias Keyboard =
    { keyboard : Vector8 Int
    , kempston : List Kempston
    }


type KeyEvent
    = KeyDownEvent Char
    | ControlKeyDownEvent ControlKey


constructor : Keyboard
constructor =
    Keyboard (Vector8.repeat 0xFF) []



--	private int ear = 0x1BBA4; // EAR noise
--
--	public int in(int port)
--	{
--		cont_port(port);
--
--		if((port&0x00E0)==0)
--			return kempston;
--		if((port&0xC002)==0xC000 && ay_enabled) {
--			if(ay_idx>=14 && (ay_reg[7]>>ay_idx-8 & 1) == 0)
--				return 0xFF;
--			return ay_reg[ay_idx];
--		}
--		int v = 0xFF;
--		if((port&0x0001)==0) {
--			for(int i=0; i<8; i++)
--				if((port&0x100<<i) == 0)
--					v &= keyboard[i];
--			int e = 0;
--			// Apply tape noise only when SPK==0 and MIC==1.
--			// thanks Jose Luis for bug report
--			if((ula28&0x18)==8) {
--				e = ear - 0x100000;
--				if((e&0xFFF00000)==0)
--					e = e<<2 | e>>18;
--				ear = e;
--			}
--			v &= ula28<<2 | e | 0xBF;
--		} else if(cpu.time>=0) {
--			int t = cpu.time;
--			int y = t/224;
--			t %= 224;
--			if(y<192 && t<124 && (t&4)==0) {
--				int x = t>>1 & 1 | t>>2;
--				if((t&1)==0)
--					x += y & 0x1800 | y<<2 & 0xE0 | y<<8 & 0x700;
--				else
--					x += 6144 | y<<2 & 0x3E0;
--				v = ram[x];
--			}
--		}
--		return v;
--	}
--Kempston JoyLeft/Right/Up/Down uses ^ (xor) so (0123)->(1032)->(0x02,0x01,0x08,0x04) (1<<(i^1))


kempstonMapping : Kempston -> Int
kempstonMapping kempston =
    case kempston of
        JoystickLeft ->
            0x02

        JoystickRight ->
            0x01

        JoystickUp ->
            0x08

        JoystickDown ->
            0x04

        JoystickControl ->
            0x10


z80_keyboard_input : Int -> Keyboard -> Int
z80_keyboard_input portnum keyboard =
    if Bitwise.and portnum 0xE0 == 0 then
        let
            val =
                keyboard.kempston |> List.foldl (\kemp total -> Bitwise.or (kempstonMapping kemp) total) 0
        in
        debugLog "kempston" val val

    else if Bitwise.and portnum 0x01 == 0 then
        let
            list1 =
                keyboard.keyboard |> Vector8.toList |> List.indexedMap Tuple.pair

            --debug_flag = (keyboard.keyboard |> List.sum) /= 255 * 8
            --x = if debug_flag then
            --      debug_log ("keyboard poll " ++ (portnum |> toHexString)) keyboard.keyboard Nothing
            --    else
            --      Nothing
            list2 =
                list1 |> List.filter (\( index, value_ ) -> Bitwise.and portnum (0x0100 |> shiftLeftBy index) == 0)

            list3 =
                list2 |> List.map (\( bitmask, listval ) -> listval)

            --y = if debug_flag then
            --      debug_log "list3" list3 Nothing
            --    else
            --      Nothing
        in
        List.foldl (\num total -> Bitwise.and num total) 0xFF list3

    else
        0xFF



--	/* keyboard & joystick */
--
--	public final int keyboard[] = new int[8];
--	public int kempston = 0;
--	public final KeyEvent keys[] = new KeyEvent[8];
--	static final int arrowsDefault[] = {0143, 0124, 0134, 0144};
--	int arrows[] = arrowsDefault;
--
--	void update_keyboard() {
--		for(int i=0; i<8; i++) keyboard[i] = 0xFF;
--		kempston = 0;
--
--		int m[] = new int[] {-1,-1,-1,-1,-1};
--		int s = 0;
--		synchronized(keys) {
--			for(int i=0; i<keys.length; i++) if(keys[i]!=null) {
--				int k = key(keys[i]);
--				if(k<0) continue;
--				// .......xxx row
--				// ....xxx... column
--				// ...x...... caps shift
--				// ..x....... symbol shift
--				// .x........ caps shift alone
--				// x......... symbol shift alone
--				s |= k;
--				if(k<01000)
--					pressed(k,m);
--			}
--		}
--		if((s&0300)==0) s |= s>>>3 & 0300;
--		if((s&0100)!=0) pressed(000,m);
--		if((s&0200)!=0) pressed(017,m);
--	}
--
-- 01000 (octal) === 0x0200 (hex)
m_initial = Vector5.repeat -1
initial_keyboard = Keyboard (Vector8.repeat 0xFF) []
-- Java Keyboard routines use octal - convert here to avoid transcribing mistakes
c_0300 = 0xC0
c_01000 = 0x200
c_0100 = 0x40
c_0200 = 0x80
c_0103 = 0x43
c_017 = 0x0F


update_keyboard : List KeyEvent -> Keyboard
update_keyboard keys =
    let
        -- more idiomatic to filter out the Nothings when mapping keys
        k_list =
            --keys |> List.map keyEventToValue |> List.filter (\( v, kemp ) -> v >= 0)
            keys |> List.map keyEventToValue |> List.filterMap identity

        s1 =
            k_list |> List.map Tuple.first |> List.foldl (\k total -> Bitwise.or k total) 0

        ( keyboard, mlist ) =
            k_list
                |> List.filter (\( v, kemp ) -> v < c_01000)
                |> List.foldl (\( k, kemp ) ( keyb, m_list ) -> pressed k { keyb | kempston = keyb.kempston ++ kemp } m_list) ( initial_keyboard, m_initial )

        s2 =
            if Bitwise.and s1 c_0300 == 0 then
                Bitwise.or s1 (Bitwise.and (s1 |> shiftRightBy 3) c_0300)

            else
                s1

        ( keyb2, mlist2 ) =
            if Bitwise.and s2 c_0100 /= 0 then
                pressed 0 keyboard mlist

            else
                ( keyboard, mlist )

        ( keyb3, _ ) =
            if Bitwise.and s2 c_0200 /= 0 then
                pressed c_017 keyb2 mlist2

            else
                ( keyb2, mlist2 )
    in
    keyb3



--	private final void pressed(int k, int m[])
--	{
--		int a = k&7, b = k>>>3 & 7;
--		int v = keyboard[a] & ~(1<<b);
--		int n = m[b];
--		keyboard[a] = v; m[b] = a;
--		if(n>=0) v |= keyboard[n];
--		for(n=0; n<8; n++)
--			if((keyboard[n]|v) != 0xFF)
--				keyboard[n] = v;
--	}


pressed : Int -> Keyboard -> Vector5 Int -> ( Keyboard, Vector5 Int )
pressed k keyboard mlist =
    let
        -- as we are anding with 0x07 here, the Maybe.withDefault can never happen
        a =
            Bitwise.and k 7
        vec_a =
            a |> Vector8.intToIndex |> Maybe.withDefault Vector8.Index0

        b =
            Bitwise.and (k |> shiftRightBy 3) 7
        vec_b = case b |> Vector5.intToIndex of
            Just index -> index
            Nothing -> (debugTodo "pressed" ("Error: b = " ++ String.fromInt b) Vector5.Index0)

        --v1 =
        --    case Array.get a (keyboard.keyboard |> Array.fromList) of
        --        Just value ->
        --            Bitwise.and value (1 |> shiftLeftBy b |> Bitwise.complement)
        --
        --        Nothing ->
        --            debugLog "pressed" ("v is impossible " ++ String.fromInt a) 0
        v1 = Bitwise.and (keyboard.keyboard |> Vector8.get vec_a) (1 |> shiftLeftBy b |> Bitwise.complement)

        --n =
        --    case Array.get b (mlist |> Array.fromList) of
        --        Just value ->
        --            value
        --
        --        Nothing ->
        --            debugTodo "pressed" ("n is wrong mlist size " ++ String.fromInt (mlist |> List.length) ++ " b = " ++ String.fromInt b) 0
        n = mlist |> Vector5.get vec_b

        --keyboard1 =
        --    (keyboard.keyboard |> Vector8.toList |> List.take a) ++ List.singleton v1 ++ (keyboard.keyboard |> Vector8.toList |> List.reverse |> List.take (7 - a) |> List.reverse)
        keyboard1 = keyboard.keyboard |> Vector8.set vec_a v1

        --new_mlist =
        --    (mlist |> List.take b) ++ List.singleton a ++ (mlist |> List.reverse |> List.take (4 - b) |> List.reverse)
        new_mlist = mlist |> Vector5.set vec_b a

        new_v =
            if n >= 0 then
                let
                    keyboard_n =
                        case Array.get n (keyboard.keyboard |> Vector8.toList |> Array.fromList) of
                            Just value ->
                                value

                            Nothing ->
                                debugLog "pressed" ("keyboard_n impossible" ++ String.fromInt n) 0
                in
                Bitwise.or v1 keyboard_n

            else
                v1

        keyboard2 =
            keyboard1
                |> Vector8.map
                    (\kb ->
                        if Bitwise.or kb new_v /= 0xFF then
                            new_v

                        else
                            kb
                    )
    in
    ( { keyboard | keyboard = keyboard2 }, new_mlist )



--
--	private int key(KeyEvent e)
--	{
--		int c = e.getKeyCode();
--		int a = e.getKeyChar();
--		int i = "[AQ10P\n ZSW29OL]XDE38IKMCFR47UJNVGT56YHB".indexOf((char)c);
--		if(i>=0) simple: {
--			int s = 0;
--			if(c>=KeyEvent.VK_0 && c<=KeyEvent.VK_9) {
--				if(c!=(int)a) break simple;
--				if(e.isAltDown()) s = 0100;
--			}
--			return i | s;
--		}
--		if(a != '\0') {
--			i = "\t\0\0!_\"\0\0:\0\0@);=\0\0\0\0#(\0+.?\0<$'\0-,/\0>%&\0^*".indexOf(a);
--			if(i>=0)
--				return i | 0200;
--		}
--		switch(c) {
--			case KeyEvent.VK_INSERT:
--			case KeyEvent.VK_ESCAPE: return 0103;
--			case KeyEvent.VK_KP_LEFT:
--			case KeyEvent.VK_LEFT: i=0; break;
--			case KeyEvent.VK_KP_DOWN:
--			case KeyEvent.VK_DOWN: i=3; break;
--			case KeyEvent.VK_KP_UP:
--			case KeyEvent.VK_UP: i=2; break;
--			case KeyEvent.VK_KP_RIGHT:
--			case KeyEvent.VK_RIGHT: i=1; break;
--			case KeyEvent.VK_BACK_SPACE: return 0104;
--			case KeyEvent.VK_SHIFT: return 01000;
--			case KeyEvent.VK_CONTROL: kempston |= 0x10; /* fall */
--			case KeyEvent.VK_ALT: return 02000;
--			default: return -1;
--		}
--		kempston |= 1<<(i^1);
--		return e.isAltDown() ? arrowsDefault[i] : arrows[i];
--	}
--c_SPECCY_KEYBOARD_CHARS = Dict.fromList [('[', 0), ('A', 1), etc


c_SPECCY_KEYBOARD_CHARS =
    "[AQ10P\n ZSW29OL]XDE38IKMCFR47UJNVGT56YHB" |> String.toList |> List.indexedMap (\index char -> ( char, index )) |> Dict.fromList

--	i = "\t\0\0!_\"\0\0:\0\0@);=\0\0\0\0#(\0+.?\0<$'\0-,/\0>%&\0^*".indexOf(a);
--       0 1 2 345 6 7 89 0 12345 6 7 8 901 2345 6789 0123 4567 89


c_SYMBOL_SHIFT_CHARS =
    Dict.fromList
        [ ( '\t', 0 )
        , ( '!', 3 )
        , ( '_', 4 )
        , ( '"', 5 )
        , ( ':', 8 )
        , ( '@', 11 )
        , ( ')', 12 )
        , ( ';', 13 )
        , ( '=', 14 )
        , ( '#', 19 )
        , ( '(', 20 )
        , ( '+', 22 )
        , ( '.', 23 )
        , ( '?', 24 )
        , ( '<', 26 )
        , ( '$', 27 )
        , ( '\'', 28 )
        , ( '-', 30 )
        , ( ',', 31 )
        , ( '/', 32 )
        , ( '>', 34 )
        , ( '%', 35 )
        , ( '&', 36 )
        , ( '^', 38 )
        , ( '*', 39 )
        ]



--c_ARROWS =
--    [ 0x63, 0x54, 0x5C, 0x64 ] |> Array.fromList
-- These are Java Octal constants
--	static final int arrowsDefault[] = {0143, 0124, 0134, 0144};


c_ARROW_0 =
    0x63


c_ARROW_1 =
    0x54


c_ARROW_2 =
    0x5C


c_ARROW_3 =
    0x64



-- This is function key() in Java


keyEventToValue : KeyEvent -> Maybe ( Int, List Kempston )
keyEventToValue event =
    case event of
        KeyDownEvent char ->
            let
                simple_key =
                    Dict.get char c_SPECCY_KEYBOARD_CHARS
            in
            case simple_key of
                Just thekey ->
                    Just ( thekey, [] )

                Nothing ->
                    let
                        sym_shift =
                            Dict.get char c_SYMBOL_SHIFT_CHARS
                    in
                    case sym_shift of
                        Just sym ->
                            Just ( Bitwise.or c_0200 sym, [] )

                        Nothing ->
                            Nothing

        ControlKeyDownEvent controlKey ->
            case controlKey of
    --			case KeyEvent.VK_INSERT:
    --			case KeyEvent.VK_ESCAPE: return 0103;
                Escape ->
                    Just ( c_0103, [] )

                ArrowLeft ->
                    --( c_ARROWS |> Array.get 0 |> Maybe.withDefault -1, [ JoystickLeft ] )
                    Just ( c_ARROW_0, [ JoystickLeft ] )

                ArrowDown ->
                    --( c_ARROWS |> Array.get 3 |> Maybe.withDefault -1, [ JoystickDown ] )
                    Just ( c_ARROW_3, [ JoystickDown ] )

                ArrowUp ->
                    --( c_ARROWS |> Array.get 2 |> Maybe.withDefault -1, [ JoystickUp ] )
                    Just ( c_ARROW_2, [ JoystickUp ] )

                ArrowRight ->
                    --( c_ARROWS |> Array.get 1 |> Maybe.withDefault -1, [ JoystickRight ] )
                    Just ( c_ARROW_1, [ JoystickRight ] )

                Backspace ->
                    Just ( 0x44, [] )

                Shift ->
                    Just ( 0x0200, [] )

                Control ->
                    Just ( 0x0400, [ JoystickControl ] )

                Alt ->
                    Just ( 0x0400, [] )

                -- index 6 in c_SPECCY_KEYBOARD_CHARS
                Enter ->
                    Just ( 6, [] )



--	public void setArrows(String s) {
--		arrows = new int[4];
--		for(int i=0; i<4; i++) {
--			int c = -1;
--			if(i<s.length())
--				c = "Caq10pE_zsw29olSxde38ikmcfr47ujnvgt56yhb"
--					.indexOf(s.charAt(i));
--			if(c<0) c = arrowsDefault[i];
--			arrows[i] = c;
--		}
--	}
-- need to do case-insensitive check as keyboard
-- can come in as Z and leave as z - but also , -> < and ? -> /(oh dear)
-- convert to upper case on way in - really want physical key (ish)


keyNotEqual : KeyEvent -> Char -> Bool
keyNotEqual event character =
    case event of
        KeyDownEvent char ->
            char /= character

        ControlKeyDownEvent _ ->
            True


keyDownEvent : Char -> List KeyEvent -> List KeyEvent
keyDownEvent character keys =
    let
        upperchar =
            character |> toUpper

        event =
            KeyDownEvent upperchar

        newkeys =
            event :: keys
    in
    debugLog ("key down " ++ (character |> fromChar) ++ " newkeys ") newkeys newkeys


keyUpEvent : Char -> List KeyEvent -> List KeyEvent
keyUpEvent character keys =
    let
        upperchar =
            character |> toUpper

        newkeys =
            keys |> List.filter (\item -> keyNotEqual item upperchar)
    in
    debugLog ("key up " ++ (character |> fromChar) ++ " newkeys ") newkeys newkeys


ctrlKeyNotEqual : KeyEvent -> ControlKey -> Bool
ctrlKeyNotEqual event str =
    case event of
        KeyDownEvent _ ->
            True

        ControlKeyDownEvent string ->
            str /= string


ctrlKeyDownEvent : String -> List KeyEvent -> List KeyEvent
ctrlKeyDownEvent string keys =
    let
        control_key =
            c_CONTROL_KEY_MAP |> Dict.get string

        newkeys =
            case control_key of
                Just a ->
                    ControlKeyDownEvent a :: keys

                Nothing ->
                    keys
    in
    debugLog ("control key down " ++ string ++ " newkeys ") newkeys newkeys


ctrlKeyUpEvent : String -> List KeyEvent -> List KeyEvent
ctrlKeyUpEvent string keys =
    let
        maybe_control_key =
            c_CONTROL_KEY_MAP |> Dict.get string

        newkeys =
            case maybe_control_key of
                Just control_key ->
                    keys |> List.filter (\item -> ctrlKeyNotEqual item control_key)

                Nothing ->
                    keys
    in
    debugLog ("control key up " ++ string ++ " newkeys ") newkeys newkeys
