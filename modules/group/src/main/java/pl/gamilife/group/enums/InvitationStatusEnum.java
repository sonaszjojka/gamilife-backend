package pl.gamilife.group.enums;


import lombok.Getter;

import java.util.Arrays;

@Getter
public enum InvitationStatusEnum {
    SENT(1),
    ACCEPTED(2),
    DECLINED(3),
    REVOKED(4);

    private final int id;

    InvitationStatusEnum(int id) {
        this.id = id;
    }

    public static InvitationStatusEnum fromId(int id) {
        return Arrays.stream(values())
                .filter(s -> s.id == id)
                .findFirst()
                .orElseThrow(() -> new IllegalArgumentException("Unknown InvitationStatus id: " + id));
    }
}
