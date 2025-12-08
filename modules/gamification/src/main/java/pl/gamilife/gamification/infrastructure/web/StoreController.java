package pl.gamilife.gamification.infrastructure.web;

import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import pl.gamilife.gamification.application.usecase.getstoreitems.getall.GetStoreItemsCommand;
import pl.gamilife.gamification.application.usecase.getstoreitems.getall.GetStoreItemsResult;
import pl.gamilife.gamification.application.usecase.getstoreitems.getall.GetStoreItemsUseCase;

import java.util.UUID;

@RestController
@RequestMapping("/api/v1/store")
public class StoreController {

    private final GetStoreItemsUseCase getStoreItemsUseCase;

    public StoreController(GetStoreItemsUseCase getStoreItemsUseCase) {
        this.getStoreItemsUseCase = getStoreItemsUseCase;
    }

    @GetMapping("/items")
    public ResponseEntity<GetStoreItemsResult> getFilteredItems(
            @RequestParam(required = false) String itemName,
            @RequestParam(required = false) Integer itemSlot,
            @RequestParam(required = false) Integer rarity,
            @RequestParam(defaultValue = "0") @Min(0) Integer page,
            @RequestParam(defaultValue = "10") @Min(1) @Max(100) Integer size
    ) {
        GetStoreItemsCommand cmd = new GetStoreItemsCommand(
                itemName,
                itemSlot,
                rarity,
                page,
                size
        );
        return ResponseEntity.ok(getStoreItemsUseCase.execute(cmd));
    }

    @GetMapping("/items/{itemId}")
    public ResponseEntity<String> getItemDetails(@PathVariable UUID itemId) {
        return ResponseEntity.ok(String.format("Hello World, Store Controller! Item ID: %s", itemId));
    }

}
