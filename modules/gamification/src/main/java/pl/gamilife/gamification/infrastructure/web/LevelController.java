package pl.gamilife.gamification.infrastructure.web;

import lombok.AllArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import pl.gamilife.gamification.application.usecase.getalllevelwithrewards.GetAllLevelsWithRewardsCommand;
import pl.gamilife.gamification.application.usecase.getalllevelwithrewards.GetAllLevelsWithRewardsResult;
import pl.gamilife.gamification.application.usecase.getalllevelwithrewards.GetAllLevelsWithRewardsUseCase;

@RestController
@AllArgsConstructor
@RequestMapping("/api/v1/levels")
public class LevelController {

    private final GetAllLevelsWithRewardsUseCase getAllLevelsWithRewardsUseCase;

    @GetMapping
    public ResponseEntity<GetAllLevelsWithRewardsResult> getLevels() {
        return ResponseEntity.ok(
                getAllLevelsWithRewardsUseCase.execute(
                        new GetAllLevelsWithRewardsCommand()
                )
        );
    }
}
