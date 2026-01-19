package pl.gamilife.gamification.domain.service.impl;

import org.instancio.Instancio;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.context.ApplicationEventPublisher;
import pl.gamilife.gamification.domain.model.Item;
import pl.gamilife.gamification.domain.model.UserInventoryItem;
import pl.gamilife.gamification.domain.port.repository.UserInventoryItemRepository;
import pl.gamilife.shared.kernel.event.ItemAcquiredEvent;

import java.util.*;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.catchThrowable;
import static org.instancio.Select.field;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class UserInventoryServiceImplTest {

    @Mock
    private UserInventoryItemRepository userInventoryItemRepository;

    @Mock
    private ApplicationEventPublisher eventPublisher;

    @InjectMocks
    private UserInventoryServiceImpl userInventoryService;

    @Test
    void shouldAddNewItemToInventory_whenItemDoesNotExist() {
        // given
        UUID userId = UUID.randomUUID();
        Item item = Instancio.of(Item.class)
                .set(field(Item::getName), "Item")
                .create();

        when(userInventoryItemRepository.findByUserIdAndItem(userId, item)).thenReturn(Optional.empty());
        when(userInventoryItemRepository.save(any(UserInventoryItem.class))).thenAnswer(invocation -> invocation.getArgument(0));

        // when
        UserInventoryItem result = userInventoryService.addItemToUsersInventory(userId, item);

        // then
        assertThat(result).isNotNull();
        assertThat(result.getUserId()).isEqualTo(userId);
        assertThat(result.getItem()).isEqualTo(item);
        assertThat(result.getQuantity()).isEqualTo(1);
        verify(userInventoryItemRepository).save(any(UserInventoryItem.class));
        verify(eventPublisher).publishEvent(any(ItemAcquiredEvent.class));
    }

    @Test
    void shouldIncrementQuantity_whenItemAlreadyExistsInInventory() {
        // given
        UUID userId = UUID.randomUUID();
        Item item = Instancio.of(Item.class)
                .set(field(Item::getName), "Item")
                .create();
        UserInventoryItem existingItem = Instancio.of(UserInventoryItem.class)
                .set(field(UserInventoryItem::getUserId), userId)
                .set(field(UserInventoryItem::getItem), item)
                .set(field(UserInventoryItem::getQuantity), 2)
                .create();

        when(userInventoryItemRepository.findByUserIdAndItem(userId, item)).thenReturn(Optional.of(existingItem));

        // when
        UserInventoryItem result = userInventoryService.addItemToUsersInventory(userId, item);

        // then
        assertThat(result.getQuantity()).isEqualTo(3);
        verify(userInventoryItemRepository).save(existingItem);
        verify(eventPublisher).publishEvent(any(ItemAcquiredEvent.class));
    }

    @Test
    void shouldThrowException_whenAddingNullItem() {
        // given
        UUID userId = UUID.randomUUID();
        Item item = null;

        // when
        Throwable thrown = catchThrowable(() -> userInventoryService.addItemToUsersInventory(userId, item));

        // then
        assertThat(thrown).isNotNull();
    }

    @Test
    void shouldAddMultipleItemsToInventory_whenMixedNewAndExistingItemsProvided() {
        // given
        UUID userId = UUID.randomUUID();
        Item item1 = Instancio.of(Item.class).set(field(Item::getName), "Item1").create();
        Item item2 = Instancio.of(Item.class).set(field(Item::getName), "Item2").create();
        Set<Item> items = Set.of(item1, item2);

        UserInventoryItem existingUii1 = Instancio.of(UserInventoryItem.class)
                .set(field(UserInventoryItem::getUserId), userId)
                .set(field(UserInventoryItem::getItem), item1)
                .set(field(UserInventoryItem::getQuantity), 1)
                .create();

        when(userInventoryItemRepository.findAllByUserIdAndItemIn(userId, items)).thenReturn(List.of(existingUii1));

        // when
        userInventoryService.addItemsToUsersInventory(userId, items);

        // then
        verify(userInventoryItemRepository).saveAll(argThat(list -> {
            boolean hasUpdatedItem1 = list.stream().anyMatch(uii -> uii.getItem().equals(item1) && uii.getQuantity() == 2);
            boolean hasNewItem2 = list.stream().anyMatch(uii -> uii.getItem().equals(item2) && uii.getQuantity() == 1);
            return list.size() == 2 && hasUpdatedItem1 && hasNewItem2;
        }));
        verify(eventPublisher).publishEvent(any(ItemAcquiredEvent.class));
    }

    @Test
    void shouldReturnEarly_whenAddItemsToUsersInventoryWithNullSet() {
        // given
        UUID userId = UUID.randomUUID();

        // when
        userInventoryService.addItemsToUsersInventory(userId, null);

        // then
        verifyNoInteractions(userInventoryItemRepository);
        verifyNoInteractions(eventPublisher);
    }

    @Test
    void shouldReturnEarly_whenAddItemsToUsersInventoryWithEmptySet() {
        // given
        UUID userId = UUID.randomUUID();

        // when
        userInventoryService.addItemsToUsersInventory(userId, Collections.emptySet());

        // then
        verifyNoInteractions(userInventoryItemRepository);
        verifyNoInteractions(eventPublisher);
    }
}
