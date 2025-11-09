package edu.pjwstk.groups.enums;

import lombok.Getter;

import java.util.Arrays;

@Getter
public enum GroupTypeEnum {
    OPEN(1),
    CLOSED(2),
    REQUEST_ONLY(3);

    private final int id;

    GroupTypeEnum(int id) {
        this.id = id;
    }

    public static GroupTypeEnum fromId(int id) {
        return Arrays.stream(values())
                .filter(t -> t.id == id)
                .findFirst()
                .orElseThrow(() -> new IllegalArgumentException("Unknown GroupType id: " + id));
    }
}
