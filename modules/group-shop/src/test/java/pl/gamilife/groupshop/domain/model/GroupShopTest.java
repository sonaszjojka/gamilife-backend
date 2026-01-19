package pl.gamilife.groupshop.domain.model;

import org.instancio.Instancio;
import org.junit.jupiter.api.Test;
import pl.gamilife.shared.kernel.exception.domain.DomainValidationException;

import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.catchThrowable;

class GroupShopTest {

    @Test
    void shouldThrowError_whenNameIsInvalid() {
        //given
        String invalidName = "";
        GroupShop groupShop = Instancio.create(GroupShop.class);

        //when
        Throwable throwable = catchThrowable(() -> groupShop.setName(invalidName));

        //then
        assertThat(throwable).isInstanceOf(DomainValidationException.class).hasMessageContaining("Name cannot be null or blank");

    }

    @Test
    void shouldThrowError_whenGroupIdIsNotProvided() {
        //given
        UUID invalidGroupId = null;
        GroupShop groupShop = Instancio.create(GroupShop.class);

        //when
        Throwable throwable = catchThrowable(() -> groupShop.setGroupId(invalidGroupId));

        //Then
        assertThat(throwable).isInstanceOf(DomainValidationException.class).hasMessageContaining("Group Id must be provided");

    }

    @Test
    void shouldThrowError_whenDescriptionIsInvalid() {
        //given
        String invalidDescription = "";
        GroupShop groupShop = Instancio.create(GroupShop.class);

        //when
        Throwable throwable = catchThrowable(() -> groupShop.setDescription(invalidDescription));

        //Then
        assertThat(throwable).isInstanceOf(DomainValidationException.class).hasMessageContaining("Description cannot be null or blank");

    }

    @Test
    void shouldThrowError_whenActiveStatusIsInvalid() {
        //Given
        Boolean activeStatus = null;
        GroupShop groupShop = Instancio.create(GroupShop.class);

        //When
        Throwable throwable = catchThrowable(() -> groupShop.setIsActive(activeStatus));

        //Then
        assertThat(throwable).isInstanceOf(DomainValidationException.class).hasMessageContaining("Active status cannot be null");
    }

    @Test
    void shouldCreateGroupShop_whenValidDataIsProvided() {
        // given
        String groupName = "Raiders";
        UUID groupId = UUID.randomUUID();

        // when
        GroupShop groupShop = GroupShop.createForGroup(groupName, groupId);

        // then
        assertThat(groupShop).isNotNull();
        assertThat(groupShop.getName()).isEqualTo("Raiders's shop");
        assertThat(groupShop.getDescription()).isEqualTo("This is a default description.");
        assertThat(groupShop.getGroupId()).isEqualTo(groupId);
        assertThat(groupShop.getIsActive()).isTrue();
    }

    @Test
    void shouldThrowException_whenNameIsNull() {
        // given
        GroupShop groupShop = Instancio.create(GroupShop.class);

        // when
        Throwable throwable = catchThrowable(() -> groupShop.setName(null));

        // then
        assertThat(throwable)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("Name cannot be null or blank");
    }

    @Test
    void shouldThrowException_whenDescriptionIsNull() {
        // given
        GroupShop groupShop = Instancio.create(GroupShop.class);

        // when
        Throwable throwable = catchThrowable(() -> groupShop.setDescription(null));

        // then
        assertThat(throwable)
                .isInstanceOf(DomainValidationException.class)
                .hasMessage("Description cannot be null or blank");
    }

    @Test
    void shouldUpdateProperties_whenSettersAreCalledWithValidData() {
        // given
        GroupShop groupShop = Instancio.create(GroupShop.class);
        String newName = "New Shop Name";
        String newDescription = "New Description";
        Boolean newStatus = !groupShop.getIsActive();

        // when
        groupShop.setName(newName);
        groupShop.setDescription(newDescription);
        groupShop.setIsActive(newStatus);

        // then
        assertThat(groupShop.getName()).isEqualTo(newName);
        assertThat(groupShop.getDescription()).isEqualTo(newDescription);
        assertThat(groupShop.getIsActive()).isEqualTo(newStatus);
    }
}
