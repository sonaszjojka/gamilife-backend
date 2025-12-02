package pl.gamilife.gamification.controller;

import pl.gamilife.gamification.usecase.getallitemrarities.GetAllItemRaritiesCommand;
import pl.gamilife.gamification.usecase.getallitemrarities.GetAllItemRaritiesResult;
import pl.gamilife.gamification.usecase.getallitemrarities.GetAllItemRaritiesUseCase;
import lombok.AllArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@AllArgsConstructor
@RequestMapping("/api/v1/item-rarities")
public class RarityController {

    private final GetAllItemRaritiesUseCase getAllItemRaritiesUseCase;

    @GetMapping
    public ResponseEntity<GetAllItemRaritiesResult> getRarities() {
        return ResponseEntity.ok(
                getAllItemRaritiesUseCase.execute(
                        new GetAllItemRaritiesCommand()
                )
        );
    }
}
