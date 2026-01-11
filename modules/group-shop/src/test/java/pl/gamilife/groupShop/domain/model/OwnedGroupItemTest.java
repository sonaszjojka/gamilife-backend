package pl.gamilife.groupShop.domain.model;
import org.instancio.Instancio;
import org.junit.jupiter.api.Test;
import pl.gamilife.groupshop.domain.model.GroupItem;
import pl.gamilife.groupshop.domain.model.OwnedGroupItem;
import pl.gamilife.shared.kernel.exception.domain.DomainValidationException;

import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.within;
import static org.assertj.core.api.AssertionsForClassTypes.catchThrowable;

class OwnedGroupItemTest {

    @Test
    void shouldThrowError_whenGroupMemberIdIsInvalid()
    {
        //Given
        GroupItem groupItem = Instancio.create(GroupItem.class);
        UUID invalidGroupMemberID = null;

        //when
        Throwable throwable = catchThrowable(()-> OwnedGroupItem.create(invalidGroupMemberID,groupItem));

        //Then
        assertThat(throwable).isInstanceOf(DomainValidationException.class).hasMessageContaining("Group Member Id cannot be null");
    }

    @Test
    void shouldThrowError_whenGroupItemIsInvalid()
    {
        //Given
        UUID groupMemberId = UUID.randomUUID();
        GroupItem groupItem = null;

        //When
        Throwable throwable = catchThrowable(()-> OwnedGroupItem.create(groupMemberId,groupItem));

        //Then
        assertThat(throwable).isInstanceOf(DomainValidationException.class).hasMessageContaining("Group Item In Shop cannot be null");
    }

    @Test
    void shouldThrowError_whenItemIsAlreadyUsed()
    {
        //Given
        OwnedGroupItem ownedGroupItem = OwnedGroupItem.create(UUID.randomUUID(),Instancio.create(GroupItem.class));
        ownedGroupItem.useItem();

        //When
        Throwable throwable = catchThrowable(ownedGroupItem::useItem);

        //Then
        assertThat(throwable).isInstanceOf(DomainValidationException.class).hasMessageContaining("Item is already used");
    }

    @Test
    void shouldReturnCurrentTime_whenUsed()
    {
        //Given
        OwnedGroupItem ownedGroupItem = OwnedGroupItem.create(UUID.randomUUID(),Instancio.create(GroupItem.class));
        //When
        ownedGroupItem.useItem();
        //Then
        assertThat(ownedGroupItem.getUsedAt()).isCloseTo(Instant.now(),within(1, ChronoUnit.SECONDS));

    }

}

