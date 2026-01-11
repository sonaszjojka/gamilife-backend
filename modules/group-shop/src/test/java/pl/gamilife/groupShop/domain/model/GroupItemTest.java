package pl.gamilife.groupShop.domain.model;
import org.instancio.Instancio;
import org.junit.jupiter.api.Test;
import pl.gamilife.groupshop.domain.model.GroupItem;
import pl.gamilife.groupshop.domain.model.GroupShop;
import pl.gamilife.shared.kernel.exception.domain.DomainValidationException;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.catchThrowable;

class GroupItemTest {

    @Test
    void shouldThrowException_whenNameIsNull()
    {
        //given
        String invalidName = null;
        GroupShop groupShop = Instancio.create(GroupShop.class);

        //when
        Throwable throwable = catchThrowable(() -> GroupItem.createPrivate(invalidName,100,true,groupShop));

        //then
        assertThat(throwable).isInstanceOf(DomainValidationException.class).hasMessageContaining("Item name must be provided");

    }

    @Test
    void shouldThrowException_whenNameIsTooLong()
    {
        //given
        String invalidName = "Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam";
        GroupShop groupShop = Instancio.create(GroupShop.class);

        //when
        Throwable throwable = catchThrowable(() -> GroupItem.createPrivate(invalidName,100,true,groupShop));

        //then
        assertThat(throwable).isInstanceOf(DomainValidationException.class).hasMessageContaining("Item name cannot exceed 255 characters");
    }

    @Test
    void shouldThrowException_whenPriceIsNotPositive()
    {

        //given
        GroupShop groupShop = Instancio.create(GroupShop.class);
        Integer invalidPrice = -1;

        //when
        Throwable throwable = catchThrowable(() -> GroupItem.createPrivate("Name",invalidPrice,true,groupShop));

        //then
        assertThat(throwable).isInstanceOf(DomainValidationException.class).hasMessageContaining("Price cannot be negative");

    }
    @Test
    void shouldThrowException_whenPriceIsNull()
    {
        //given
        GroupShop groupShop = Instancio.create(GroupShop.class);
        Integer invalidPrice = null;

        //when
        Throwable throwable = catchThrowable(() -> GroupItem.createPrivate("Name",invalidPrice,true,groupShop));

        //then
        assertThat(throwable).isInstanceOf(DomainValidationException.class).hasMessageContaining("Price must be provided");
    }
    @Test
    void shouldThrowException_whenPriceIsToHigh()
    {
        //given
        GroupShop groupShop = Instancio.create(GroupShop.class);
        Integer invalidPrice = 999999999;

        //when
        Throwable throwable = catchThrowable(() -> GroupItem.createPrivate("Name",invalidPrice,true,groupShop));

        //then
        assertThat(throwable).isInstanceOf(DomainValidationException.class).hasMessageContaining("Price cannot exceed 10000");
    }
    @Test
    void shouldThrowException_whenActiveStatusIsNull()
    {
        //given
        GroupShop groupShop = Instancio.create(GroupShop.class);
        Boolean invalidIsActive = null;

        //when
        Throwable throwable = catchThrowable(() -> GroupItem.createPrivate("Name",9,invalidIsActive,groupShop));

        //then
        assertThat(throwable).isInstanceOf(DomainValidationException.class).hasMessageContaining("Active status must be provided");
    }
    @Test
    void shouldThrowException_whenGroupShopIsNotProvided()
    {
        //given
        GroupShop groupShop = null;

        //when
        Throwable throwable = catchThrowable(() -> GroupItem.createPrivate("Name",9,Boolean.TRUE,groupShop));

        //then
        assertThat(throwable).isInstanceOf(DomainValidationException.class).hasMessageContaining("GroupShop must be provided");
    }


}
