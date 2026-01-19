package pl.gamilife.gamification.application.usecase.getuserstatistics;

import java.io.Serializable;
import java.util.UUID;

public record GetUserStatisticsResult(
        UUID id,
        StatisticType statisticType,
        Integer count

) implements Serializable {
    public record StatisticType(
            Integer id,
            String type

    ) implements Serializable {

    }
}

