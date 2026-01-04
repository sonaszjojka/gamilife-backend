package pl.gamilife.gamification.infrastructure.web;

import lombok.AllArgsConstructor;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import pl.gamilife.gamification.application.usecase.getuserstatistics.GetUserStatisticsCommand;
import pl.gamilife.gamification.application.usecase.getuserstatistics.GetUserStatisticsResult;
import pl.gamilife.gamification.application.usecase.getuserstatistics.GetUserStatisticsUseCase;

import java.util.UUID;

@RestController
@AllArgsConstructor
@RequestMapping("/api/v1/users/{userId}/statistics")
public class UserStatisticsController {
    private final GetUserStatisticsUseCase getUserStatisticsUseCase;

    @GetMapping
    public GetUserStatisticsResult getUserStatistics(@PathVariable UUID userId) {


        return getUserStatisticsUseCase.execute(new GetUserStatisticsCommand(userId));
    }

}
