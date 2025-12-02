package pl.gamilife.infrastructure.core.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@AllArgsConstructor
public enum StatisticTypeEnum {
    COMPLETED_TASKS(1),
    HABIT_STREAK(2),
    POMODORO_TASKS_COMPLETED(3),
    GROUP_TASKS_COMPLETED(4),
    JOINED_GROUPS(5),
    OWNED_ITEMS(6),
    ITEMS_PURCHASED(7),
    GROUP_ITEMS_PURCHASED(8);

    private static final Map<Integer, StatisticTypeEnum> BY_ID =
            Stream.of(values()).collect(Collectors.toMap(StatisticTypeEnum::getStatisticTypeId, e -> e));
    @Getter
    private final Integer statisticTypeId;

    public static StatisticTypeEnum fromId(int statisticTypeId) {
        StatisticTypeEnum statisticTypeEnum = BY_ID.get(statisticTypeId);
        if (statisticTypeEnum == null) {
            throw new IllegalArgumentException("Invalid statisticTypeId: " + statisticTypeId);
        }
        return statisticTypeEnum;
    }
}
