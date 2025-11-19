package edu.pjwstk.gamification.controller;

import edu.pjwstk.gamification.usecase.getallitemslots.GetAllItemSlotsCommand;
import edu.pjwstk.gamification.usecase.getallitemslots.GetAllItemSlotsResult;
import edu.pjwstk.gamification.usecase.getallitemslots.GetAllItemSlotsUseCase;
import lombok.AllArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

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
