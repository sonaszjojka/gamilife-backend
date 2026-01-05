package pl.gamilife.gamification.infrastructure.web;

import lombok.AllArgsConstructor;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import pl.gamilife.gamification.application.usecase.getuserstatistics.GetUserStatisticsCommand;
import pl.gamilife.gamification.application.usecase.getuserstatistics.GetUserStatisticsResult;
import pl.gamilife.gamification.application.usecase.getuserstatistics.GetUserStatisticsUseCase;
import pl.gamilife.shared.web.security.annotation.CurrentUserId;

import java.util.List;
import java.util.UUID;

@RestController
@AllArgsConstructor
@RequestMapping("/api/v1/users/stats")
public class UserStatisticsController {
    private final GetUserStatisticsUseCase getUserStatisticsUseCase;

    @GetMapping
    public List<GetUserStatisticsResult> getUserStatistics(@CurrentUserId UUID userId) {


        return getUserStatisticsUseCase.execute(new GetUserStatisticsCommand(userId));
    }

}
