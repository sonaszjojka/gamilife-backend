package pl.gamilife.communication.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum NotificationType {
    ACHIEVEMENT_UNLOCKED(1),
    ITEM_ACQUIRED(2),
    LEVEL_UP(3),
    GROUP_INVITATION(4),
    GROUP_ITEM_USED(5),
    NEW_GROUP_MEMBER(6),
    GROUP_MEMBER_LEFT(7),
    NEW_GROUP_MESSAGE(8),
    GROUP_TASK_ASSIGNED(9),
    GROUP_TASK_COMPLETED(10),
    GROUP_REQUEST_STATUS_UPDATED(11),
    NEW_GROUP_REQUEST(12),
    OTHER(99),
    GAMIFICATION_VALUES_CHANGED(100);

    private final int id;

    public static NotificationType fromId(int id) {
        for (NotificationType type : values()) {
            if (type.getId() == id) {
                return type;
            }
        }
        return OTHER;
    }
}
