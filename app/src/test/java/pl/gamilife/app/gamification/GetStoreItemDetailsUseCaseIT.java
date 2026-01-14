package pl.gamilife.app.gamification;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import pl.gamilife.app.BaseIntegrationTest;
import pl.gamilife.gamification.application.usecase.getstoreitems.getbyid.GetStoreItemDetailsCommand;
import pl.gamilife.gamification.application.usecase.getstoreitems.getbyid.GetStoreItemDetailsUseCase;
import pl.gamilife.gamification.application.usecase.getstoreitems.getbyid.StoreItemDetailsDto;
import pl.gamilife.gamification.domain.exception.ItemNotFoundException;
import pl.gamilife.gamification.domain.model.Item;
import pl.gamilife.gamification.infrastructure.persistence.jpa.JpaItemRepository;
import pl.gamilife.user.persistence.User;

import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.catchThrowableOfType;

class GetStoreItemDetailsUseCaseIT extends BaseIntegrationTest {

    @Autowired
    private GetStoreItemDetailsUseCase getStoreItemDetailsUseCase;

    @Autowired
    private JpaItemRepository itemRepository;

    @Test
    @DisplayName("Should return store item details")
    void shouldReturnStoreItemDetails() {
        // given
        User user = createUserWithStats();
        Item item = itemRepository.findAll().stream().findFirst().orElseThrow();

        // when
        StoreItemDetailsDto result = getStoreItemDetailsUseCase.execute(
                new GetStoreItemDetailsCommand(item.getId(), user.getId())
        );

        flushAndClear();

        // then
        assertThat(result.id()).isEqualTo(item.getId());
        assertThat(result.name()).isEqualTo(item.getName());
    }

    @Test
    @DisplayName("Should throw exception when item not found")
    void shouldThrowException_whenItemNotFound() {
        // given
        User user = createUserWithStats();
        UUID itemId = UUID.randomUUID();

        // when
        GetStoreItemDetailsCommand cmd = new GetStoreItemDetailsCommand(itemId, user.getId());
        Throwable throwable = catchThrowableOfType(
                ItemNotFoundException.class,
                () -> getStoreItemDetailsUseCase.execute(cmd)
        );

        flushAndClear();

        // then
        assertThat(throwable).isNotNull()
                .isInstanceOf(ItemNotFoundException.class);
    }
}