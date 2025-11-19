package edu.pjwstk.gamification.api;

import edu.pjwstk.api.gamification.GamificationApi;
import edu.pjwstk.gamification.usecase.inituserstatistics.InitUserStatisticsCommand;
import edu.pjwstk.gamification.usecase.inituserstatistics.InitUserStatisticsUseCase;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
@AllArgsConstructor
public class GamificationApiImpl implements GamificationApi {

    private final InitUserStatisticsUseCase initUserStatisticsUseCase;

    @Override
    public void initUserStatisticsFor(UUID userId) {
        initUserStatisticsUseCase.execute(new InitUserStatisticsCommand(userId));
    }
}
