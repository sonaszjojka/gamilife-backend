package pl.gamilife.gamification.application.usecase.getstoreitems.getbyid;


import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.gamification.domain.exception.ItemNotFoundException;
import pl.gamilife.gamification.domain.model.Item;
import pl.gamilife.gamification.infrastructure.persistence.ItemRepositoryAdapter;
import pl.gamilife.gamification.infrastructure.persistence.UserInventoryItemRepositoryAdapter;

import java.util.UUID;

@Service
@Transactional(readOnly = true)
@RequiredArgsConstructor
@Slf4j
public class GetStoreItemDetailsUseCaseImpl implements GetStoreItemDetailsUseCase {
    private final ItemRepositoryAdapter itemRepository;
    private final UserInventoryItemRepositoryAdapter itemInventoryItemRepository;


    @Override
    public StoreItemDetailsDto execute(UUID itemId,UUID userId) {

        Item item = itemRepository.findById(itemId).orElseThrow(() ->
                new ItemNotFoundException("Item with id: " +itemId + " not found")
        );
        boolean isOwned= itemInventoryItemRepository.findByUserIdAndItem(userId, item).isPresent();


        return toDto(item, isOwned);
    }

    private StoreItemDetailsDto toDto(Item item, Boolean isOwned ) {
        return new StoreItemDetailsDto(
                item.getId(),
                item.getName(),
                item.getImagePath(),
                new StoreItemDetailsDto.ItemSlotDto(
                        item.getItemSlot().getId(),
                        item.getItemSlot().getName()
                ),
                new StoreItemDetailsDto.RarityDto(
                        item.getRarity().getId(),
                        item.getRarity().getName()
                ),
                item.getPrice(),
                item.getDescription(),
                isOwned
        );
    }
}
