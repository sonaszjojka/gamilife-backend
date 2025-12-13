package pl.gamilife.gamification.infrastructure.web;

import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import pl.gamilife.gamification.application.usecase.getstoreitems.getall.GetStoreItemsCommand;
import pl.gamilife.gamification.application.usecase.getstoreitems.getall.GetStoreItemsUseCase;
import pl.gamilife.gamification.application.usecase.getstoreitems.getall.StoreItemDto;
import pl.gamilife.gamification.application.usecase.getstoreitems.getbyid.GetStoreItemDetailsCommand;
import pl.gamilife.gamification.application.usecase.getstoreitems.getbyid.GetStoreItemDetailsUseCase;
import pl.gamilife.gamification.application.usecase.getstoreitems.getbyid.StoreItemDetailsDto;
import pl.gamilife.shared.kernel.architecture.Page;
import pl.gamilife.shared.web.security.annotation.CurrentUserId;

import java.util.List;
import java.util.UUID;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/v1/store")
public class StoreController {

    private final GetStoreItemsUseCase getStoreItemsUseCase;
    private final GetStoreItemDetailsUseCase getStoreItemDetailsUseCase;

    @GetMapping("/item")
    public ResponseEntity<Page<StoreItemDto>> getFilteredItems(
            @RequestParam(required = false) String itemName,
            @RequestParam(required = false) List<Integer> itemSlot,
            @RequestParam(required = false) List<Integer> rarity,
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

    @GetMapping("/item/{itemId}")
    public ResponseEntity<StoreItemDetailsDto> getItemDetails(@PathVariable UUID itemId, @CurrentUserId UUID userId) {

        return ResponseEntity.ok(getStoreItemDetailsUseCase.execute(new GetStoreItemDetailsCommand(itemId, userId)));
    }

}
