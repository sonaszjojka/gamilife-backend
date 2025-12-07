package pl.gamilife.gamification.infrastructure.web;

import lombok.AllArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import pl.gamilife.gamification.application.usecase.getallitemslots.GetAllItemSlotsCommand;
import pl.gamilife.gamification.application.usecase.getallitemslots.GetAllItemSlotsResult;
import pl.gamilife.gamification.application.usecase.getallitemslots.GetAllItemSlotsUseCase;

@RestController
@AllArgsConstructor
@RequestMapping("/api/v1/item-slots")
public class ItemSlotController {

    private final GetAllItemSlotsUseCase getAllItemSlotsUseCase;

    @GetMapping
    public ResponseEntity<GetAllItemSlotsResult> getItemSlots() {
        return ResponseEntity.ok(
                getAllItemSlotsUseCase.execute(
                        new GetAllItemSlotsCommand()
                )
        );
    }
}
