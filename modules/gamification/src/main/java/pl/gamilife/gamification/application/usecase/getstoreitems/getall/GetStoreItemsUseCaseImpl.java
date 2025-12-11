package pl.gamilife.gamification.application.usecase.getstoreitems.getall;


import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.stereotype.Service;
import pl.gamilife.gamification.domain.model.Item;
import pl.gamilife.gamification.domain.model.enums.ItemSlotEnum;
import pl.gamilife.gamification.domain.model.enums.RarityEnum;
import pl.gamilife.gamification.domain.model.filter.StoreItemsFilter;
import pl.gamilife.gamification.infrastructure.persistence.ItemRepositoryAdapter;
import pl.gamilife.shared.kernel.architecture.Page;

@Service
@Transactional(readOnly = true)
@RequiredArgsConstructor
@Slf4j
public class GetStoreItemsUseCaseImpl implements GetStoreItemsUseCase {

    private final ItemRepositoryAdapter itemRepositoryAdapter;

    @Override
    public Page<StoreItemDto> execute(GetStoreItemsCommand cmd) {


        ItemSlotEnum[] itemSlotEnum = cmd.itemSlot() != null
                ? ItemSlotEnum.fromIds(cmd.itemSlot())
                : null;

        RarityEnum[] rarityEnum = cmd.rarity() != null
                ? RarityEnum.fromIds(cmd.rarity())
                : null;

        Page<Item> itemPage = itemRepositoryAdapter.findAll(
                new StoreItemsFilter(cmd.itemName(), itemSlotEnum, rarityEnum),
                cmd.page(),
                cmd.size()
        );
        return  itemPage.map(this::toDto);
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
