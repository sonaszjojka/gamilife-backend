package pl.gamilife.groupShop.domain.model;
import org.instancio.Instancio;
import org.junit.jupiter.api.Test;
import pl.gamilife.groupshop.domain.model.GroupShop;
import pl.gamilife.shared.kernel.exception.domain.DomainValidationException;

import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.catchThrowable;

class GroupShopTest {

@Test
    void shouldThrowError_whenNameIsInvalid()
{
    //given
    String invalidName = "";
    GroupShop groupShop = Instancio.create(GroupShop.class);

    //when
    Throwable throwable = catchThrowable(() -> groupShop.setName(invalidName));

    //then
    assertThat(throwable).isInstanceOf(DomainValidationException.class).hasMessageContaining("Name cannot be null or blank");

}

@Test
    void shouldThrowError_whenGroupIdIsNotProvided()
{
    //given
    UUID invalidGroupId = null;
    GroupShop groupShop = Instancio.create(GroupShop.class);

    //when
    Throwable throwable = catchThrowable(() -> groupShop.setGroupId(invalidGroupId));

    //Then
    assertThat(throwable).isInstanceOf(DomainValidationException.class).hasMessageContaining("Group Id must be provided");

}

@Test
    void shouldThrowError_whenDescriptionIsInvalid()
{
    //given
    String invalidDescription = "";
    GroupShop groupShop = Instancio.create(GroupShop.class);

    //when
    Throwable throwable = catchThrowable(() -> groupShop.setDescription(invalidDescription));

    //Then
    assertThat(throwable).isInstanceOf(DomainValidationException.class).hasMessageContaining("Description cannot be null or blank");

}

@Test
    void shouldThrowError_whenActiveStatusIsInvalid()
{
    //Given
    Boolean activeStatus = null;
    GroupShop groupShop = Instancio.create(GroupShop.class);

    //When
    Throwable throwable = catchThrowable(()->groupShop.setIsActive(activeStatus));

    //Then
    assertThat(throwable).isInstanceOf(DomainValidationException.class).hasMessageContaining("Active status cannot be null");
}
}
