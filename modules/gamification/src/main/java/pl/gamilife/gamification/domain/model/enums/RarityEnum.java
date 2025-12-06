package pl.gamilife.gamification.domain.model.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@AllArgsConstructor
public enum RarityEnum {
    COMMON(1),
    UNCOMMON(2),
    RARE(3),
    EPIC(4),
    LEGENDARY(5);

    private static final Map<Integer, RarityEnum> BY_ID =
            Stream.of(values()).collect(Collectors.toMap(RarityEnum::getRarityId, e -> e));
    @Getter
    private final int rarityId;

    public static RarityEnum fromId(int rarityId) {
        RarityEnum rarityEnum = BY_ID.get(rarityId);
        if (rarityEnum == null) {
            throw new IllegalArgumentException("Invalid rarityId: " + rarityId);
        }
        return rarityEnum;
    }
}
