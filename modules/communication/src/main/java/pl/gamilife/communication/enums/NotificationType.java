package pl.gamilife.communication.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum NotificationType {
    TASK_COMPLETED(1),
    ITEM_ACQUIRED(2),
    LEVEL_UP(3),
    GROUP_INVITATION(4),
    GROUP_ITEM_USED(5),
    OTHER(99);

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
