package pl.gamilife.gamification.application.usecase.getstoreitems.getall;


import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import pl.gamilife.gamification.domain.model.Item;
import pl.gamilife.gamification.domain.model.enums.ItemSlotEnum;
import pl.gamilife.gamification.domain.model.enums.RarityEnum;
import pl.gamilife.gamification.domain.model.filter.StoreItemsFilter;
import pl.gamilife.gamification.infrastructure.persistence.ItemRepositoryAdapter;
import pl.gamilife.shared.kernel.architecture.Page;

@Service
@Transactional()
@RequiredArgsConstructor
@Slf4j
public class GetStoreItemsUseCaseImpl implements GetStoreItemsUseCase {

    private final ItemRepositoryAdapter itemRepositoryAdapter;

    @Override
    public GetStoreItemsResult execute(GetStoreItemsCommand cmd) {


        ItemSlotEnum itemSlotEnum = cmd.itemSlot() != null
                ? ItemSlotEnum.fromId(cmd.itemSlot())
                : null;

        RarityEnum rarityEnum = cmd.rarity() != null
                ? RarityEnum.fromId(cmd.rarity())
                : null;

        Page<Item> itemPage = itemRepositoryAdapter.findAll(
                new StoreItemsFilter(cmd.itemName(), itemSlotEnum, rarityEnum),
                cmd.page(),
                cmd.size()
        );

            itemPage.map(this::toDto);
        return null;
    }

    private GetStoreItemsResult.StoreItemDto toDto(Item item) {
        return new GetStoreItemsResult.StoreItemDto(
                item.getId(),
                item.getName(),
                item.getImagePath(),
                new GetStoreItemsResult.ItemSlotDto(
                        item.getItemSlot().getId(),
                        item.getItemSlot().getName()
                ),
                new GetStoreItemsResult.RarityDto(
                        item.getRarity().getId(),
                        item.getRarity().getName()
                ),
                item.getPrice()
        );
    }
}
