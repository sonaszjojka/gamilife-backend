package edu.pjwstk.notification.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum NotificationType {
    TASK_COMPLETED(1),
    ITEM_ACQUIRED(2),
    LEVEL_UP(3),
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
