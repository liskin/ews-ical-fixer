import Data.Default
import Data.Time.LocalTime
import Text.ICalendar
import qualified Data.ByteString.Lazy as L
import qualified Data.Map.Lazy as M

main = do
    input <- L.getContents
    let Right ([cal], _) = parseICalendar def "stdin" input
    L.putStr $ printICalendar def $ fixEvents cal

fixEvents :: VCalendar -> VCalendar
fixEvents cal = cal{ vcEvents = es' }
    where
        es = vcEvents cal
        es' = M.mapWithKey fixEvent es

        fixEvent (_, Nothing) e = e
        fixEvent (uid, _) e =
            case M.lookup (uid, Nothing) es of
                Just e1 -> fixRecurId e e1
                Nothing -> e

-- http://www.kanzaki.com/docs/ical/recurrenceId.html says:
--
-- > This property is used in conjunction with the "UID" and "SEQUENCE"
-- > property to identify a specific instance of a recurring "VEVENT", "VTODO"
-- > or "VJOURNAL" calendar component. The property value is the effective
-- > value of the "DTSTART" property of the recurrence instance.
--
-- Exchange does not set it to a correct effective value, it only sets the
-- day, not time. This function fixes this.
fixRecurId e@VEvent{ veRecurId = Just recId@RecurrenceIdDateTime{ recurrenceIdDateTime = recDT } }
    e1@VEvent{ veDTStart = Just DTStartDateTime{ dtStartDateTimeValue = startDT } } =
        e{ veRecurId = Just recId{ recurrenceIdDateTime = fixRecurTime recDT startDT } }
fixRecurId e _ = e

fixRecurTime recDT@ZonedDateTime{ dateTimeFloating = recDTF }
    startDT@ZonedDateTime{ dateTimeFloating = LocalTime{ localTimeOfDay = startTimeOfDay } } =
        recDT{ dateTimeFloating = recDTF{ localTimeOfDay = startTimeOfDay } }
