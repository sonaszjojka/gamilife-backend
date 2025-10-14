package edu.pjwstk.groups.shared;

import java.util.Arrays;

public enum GroupTypeEnum {
    OPEN(1),
    CLOSED(2),
    REQUEST_ONLY(3);

    private final int id;

    GroupTypeEnum(int id) { this.id = id; }

    public int getId() { return id; }

    public static GroupTypeEnum fromId(int id) {
        return Arrays.stream(values())
                .filter(t -> t.id == id)
                .findFirst()
                .orElseThrow(() -> new IllegalArgumentException("Unknown GroupType id: " + id));
    }
}
