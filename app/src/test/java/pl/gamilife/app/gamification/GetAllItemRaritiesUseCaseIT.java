package pl.gamilife.app.gamification;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import pl.gamilife.app.BaseIntegrationTest;
import pl.gamilife.gamification.application.usecase.getallitemrarities.GetAllItemRaritiesCommand;
import pl.gamilife.gamification.application.usecase.getallitemrarities.GetAllItemRaritiesResult;
import pl.gamilife.gamification.application.usecase.getallitemrarities.GetAllItemRaritiesUseCase;

import static org.assertj.core.api.Assertions.assertThat;

class GetAllItemRaritiesUseCaseIT extends BaseIntegrationTest {

    @Autowired
    private GetAllItemRaritiesUseCase getAllItemRaritiesUseCase;

    @Test
    @DisplayName("Should return all item rarities")
    void shouldReturnAllItemRarities() {
        // when
        GetAllItemRaritiesResult result = getAllItemRaritiesUseCase.execute(new GetAllItemRaritiesCommand());

        flushAndClear();

        // then
        assertThat(result.itemRarities()).isNotEmpty();
        assertThat(result.itemRarities()).extracting(GetAllItemRaritiesResult.ItemRarityDTO::name)
                .contains("Common", "Uncommon", "Rare", "Epic", "Legendary");
    }
}