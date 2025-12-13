package pl.gamilife.gamification.application.usecase.getstoreitems.getall;


import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.gamification.domain.model.Item;
import pl.gamilife.gamification.domain.model.enums.ItemSlotEnum;
import pl.gamilife.gamification.domain.model.enums.RarityEnum;
import pl.gamilife.gamification.domain.model.filter.StoreItemsFilter;
import pl.gamilife.gamification.domain.port.repository.ItemRepository;
import pl.gamilife.shared.kernel.architecture.Page;

@Service
@Transactional(readOnly = true)
@RequiredArgsConstructor
@Slf4j
public class GetStoreItemsUseCaseImpl implements GetStoreItemsUseCase {

    private final ItemRepository itemRepository;

    @Override
    public Page<StoreItemDto> execute(GetStoreItemsCommand cmd) {


        if (cmd.itemSlot() != null) {
            cmd.itemSlot().forEach(ItemSlotEnum::fromId);
        }

        if (cmd.rarity() != null) {
            cmd.rarity().forEach(RarityEnum::fromId);
        }

        Page<Item> itemPage = itemRepository.findAll(
                new StoreItemsFilter(cmd.itemName(), cmd.itemSlot(), cmd.rarity()),
                cmd.page(),
                cmd.size()
        );
        return itemPage.map(this::toDto);
    }

    private StoreItemDto toDto(Item item) {
        return new StoreItemDto(
                item.getId(),
                item.getName(),
                item.getImagePath(),
                new StoreItemDto.ItemSlotDto(
                        item.getItemSlot().getId(),
                        item.getItemSlot().getName()
                ),
                new StoreItemDto.RarityDto(
                        item.getRarity().getId(),
                        item.getRarity().getName()
                ),
                item.getPrice()
        );
    }
}
