package pl.gamilife.gamification.application.usecase.getstoreitems.getbyid;


import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.gamification.domain.exception.ItemNotFoundException;
import pl.gamilife.gamification.domain.model.Item;
import pl.gamilife.gamification.domain.port.repository.ItemRepository;
import pl.gamilife.gamification.domain.port.repository.UserInventoryItemRepository;

import java.util.UUID;

@Service
@Transactional(readOnly = true)
@RequiredArgsConstructor
@Slf4j
public class GetStoreItemDetailsUseCaseImpl implements GetStoreItemDetailsUseCase {
    private final ItemRepository itemRepository;
    private final UserInventoryItemRepository itemInventoryItemRepository;


    @Override
    public StoreItemDetailsDto execute(UUID itemId) {

        Item item = itemRepository.findById(itemId).orElseThrow(() ->
                new ItemNotFoundException("Item with id: " +itemId + " not found")
        );


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
                false
        );
    }
}
