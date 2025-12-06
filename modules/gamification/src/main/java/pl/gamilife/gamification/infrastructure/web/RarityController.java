package pl.gamilife.gamification.infrastructure.web;

import lombok.AllArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import pl.gamilife.gamification.application.usecase.getallitemrarities.GetAllItemRaritiesCommand;
import pl.gamilife.gamification.application.usecase.getallitemrarities.GetAllItemRaritiesResult;
import pl.gamilife.gamification.application.usecase.getallitemrarities.GetAllItemRaritiesUseCase;

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
