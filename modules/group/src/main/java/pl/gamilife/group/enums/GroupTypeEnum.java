package pl.gamilife.group.enums;

import lombok.Getter;
import pl.gamilife.group.exception.domain.GroupTypeNotFoundException;

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
                .orElseThrow(() -> new GroupTypeNotFoundException("Unknown GroupType id: " + id));
    }
}
