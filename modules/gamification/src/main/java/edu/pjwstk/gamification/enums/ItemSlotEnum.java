package edu.pjwstk.gamification.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@AllArgsConstructor
public enum ItemSlotEnum {
    HEAD(1),
    BODY(2),
    LEGS(3),
    FEET(4),
    ACCESSORY(5),
    TITLE(6),
    BADGE(7),
    BACKGROUND(8);

    private static final Map<Integer, ItemSlotEnum> BY_ID =
            Stream.of(values()).collect(Collectors.toMap(ItemSlotEnum::getItemSlotId, e -> e));
    @Getter
    private final int itemSlotId;

    public static ItemSlotEnum fromId(int itemSlotId) {
        ItemSlotEnum itemSlotEnum = BY_ID.get(itemSlotId);
        if (itemSlotEnum == null) {
            throw new IllegalArgumentException("Invalid itemSlotId: " + itemSlotId);
        }
        return itemSlotEnum;
    }
}
