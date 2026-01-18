package pl.gamilife.groupshop.domain.model;

import org.instancio.Instancio;
import org.junit.jupiter.api.Test;
import pl.gamilife.shared.kernel.exception.domain.DomainValidationException;

import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.within;
import static org.assertj.core.api.AssertionsForClassTypes.catchThrowable;

class OwnedGroupItemTest {

    @Test
    void shouldThrowError_whenGroupMemberIdIsInvalid() {
        //Given
        GroupItem groupItem = Instancio.create(GroupItem.class);
        UUID invalidGroupMemberID = null;

        //when
        Throwable throwable = catchThrowable(() -> OwnedGroupItem.create(invalidGroupMemberID, groupItem));

        //Then
        assertThat(throwable).isInstanceOf(DomainValidationException.class).hasMessageContaining("Group Member Id cannot be null");
    }

    @Test
    void shouldThrowError_whenGroupItemIsInvalid() {
        //Given
        UUID groupMemberId = UUID.randomUUID();
        GroupItem groupItem = null;

        //When
        Throwable throwable = catchThrowable(() -> OwnedGroupItem.create(groupMemberId, groupItem));

        //Then
        assertThat(throwable).isInstanceOf(DomainValidationException.class).hasMessageContaining("Group Item In Shop cannot be null");
    }

    @Test
    void shouldThrowError_whenItemIsAlreadyUsed() {
        //Given
        OwnedGroupItem ownedGroupItem = OwnedGroupItem.create(UUID.randomUUID(), Instancio.create(GroupItem.class));
        ownedGroupItem.useItem();

        //When
        Throwable throwable = catchThrowable(ownedGroupItem::useItem);

        //Then
        assertThat(throwable).isInstanceOf(DomainValidationException.class).hasMessageContaining("Item is already used");
    }

    @Test
    void shouldReturnCurrentTime_whenUsed() {
        //Given
        OwnedGroupItem ownedGroupItem = OwnedGroupItem.create(UUID.randomUUID(), Instancio.create(GroupItem.class));
        //When
        ownedGroupItem.useItem();
        //Then
        assertThat(ownedGroupItem.getUsedAt()).isCloseTo(Instant.now(), within(1, ChronoUnit.SECONDS));

    }

    @Test
    void shouldCreateOwnedGroupItem_whenValidDataIsProvided() {
        // given
        UUID groupMemberId = UUID.randomUUID();
        GroupItem groupItem = Instancio.create(GroupItem.class);

        // when
        OwnedGroupItem result = OwnedGroupItem.create(groupMemberId, groupItem);

        // then
        assertThat(result).isNotNull();
        assertThat(result.getGroupMemberId()).isEqualTo(groupMemberId);
        assertThat(result.getGroupItem()).isEqualTo(groupItem);
        assertThat(result.getGroupItemInShopId()).isEqualTo(groupItem.getId());
        assertThat(result.getUsedAt()).isNull();
    }

    @Test
    void shouldUpdateProperties_whenSettersAreCalledWithValidData() {
        // given
        OwnedGroupItem ownedItem = Instancio.create(OwnedGroupItem.class);
        UUID newMemberId = UUID.randomUUID();
        GroupItem newItem = Instancio.create(GroupItem.class);

        // when
        ownedItem.setGroupMemberId(newMemberId);
        ownedItem.setGroupItem(newItem);

        // then
        assertThat(ownedItem.getGroupMemberId()).isEqualTo(newMemberId);
        assertThat(ownedItem.getGroupItem()).isEqualTo(newItem);
        assertThat(ownedItem.getGroupItemInShopId()).isEqualTo(newItem.getId());
    }

}

