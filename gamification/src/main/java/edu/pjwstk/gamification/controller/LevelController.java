package edu.pjwstk.gamification.controller;

import edu.pjwstk.gamification.usecase.getalllevelwithrewards.GetAllLevelsWithRewardsCommand;
import edu.pjwstk.gamification.usecase.getalllevelwithrewards.GetAllLevelsWithRewardsResult;
import edu.pjwstk.gamification.usecase.getalllevelwithrewards.GetAllLevelsWithRewardsUseCase;
import lombok.AllArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

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
