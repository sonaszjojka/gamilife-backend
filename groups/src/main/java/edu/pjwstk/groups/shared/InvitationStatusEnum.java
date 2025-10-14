package edu.pjwstk.groups.shared;


import java.util.Arrays;

public enum InvitationStatusEnum {
    SENT(1),
    ACCEPTED(2),
    DECLINED(3);

    private final int id;

    InvitationStatusEnum(int id) { this.id = id; }

    public int getId() { return id; }

    public static InvitationStatusEnum fromId(int id) {
        return Arrays.stream(values())
                .filter(s -> s.id == id)
                .findFirst()
                .orElseThrow(() -> new IllegalArgumentException("Unknown InvitationStatus id: " + id));
    }
}
