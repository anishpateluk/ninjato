namespace Ninjato.Tests

open Ninjato.Engine.Services.stringExtensions
open Ninjato.Engine.Services.QueryConstruction

open Xunit
open FsUnit.Xunit

module QueryConstructionTests =

    let sample = "
        select * 
        from Tag t
        $?($$area$$ == null
        && $$area$$ != 'NaN') {
        inner join TagData td on td.Id = t.LatestRevisionId
        } $? ($$area$$ == 'someGuid'){
        inner join TagData td2 on td2.Id = t.LatestRevisionId
        inner join Area a on a.Id = td2.AreaId
        }$:{
        inner join TagData td3 on td3.Id = t.LatestRevisionId
        }
        where 1 = 1
        $?($$area$$ == 'someGuid'){
        and td2.AreaId = $$area$$ and td2.SubsystemId = $$subsystem$$
        }
        and 2 = 2"

    let sample2 = "
        select * 
        from Tag t
        $?($$area$$ == null
        && $$area$$ != 'NaN') {
        inner join TagData td on td.Id = t.LatestRevisionId
        } $? ($$area$$ == 'someGuid'){
        inner join TagData td2 on td2.Id = t.LatestRevisionId
        inner join Area a on a.Id = td2.AreaId
        }$:{
        inner join TagData td3 on td3.Id = t.LatestRevisionId
        }
        where 1 = 1
        $?($$area$$ == 'someGuid'){
        and td2.AreaId = $$area$$
        }
        and 2 = 2"

    let sample3 = "
        select * 
        from Tag t
        $?($$area$$ == null) {
        inner join TagData td on td.Id = t.LatestRevisionId
        } $? ($$area$$ == 'someGuid'){
        inner join TagData td2 on td2.Id = t.LatestRevisionId
        inner join Area a on a.Id = td2.AreaId
        }$:{
        inner join TagData td3 on td3.Id = t.LatestRevisionId
        }
        $?($$area$$ == 'someGuid2'){
        inner join TagData td4 on td4.Id = t.LatestRevisionId
        inner join Area a2 on a2.Id = td4.AreaId
        }$:{
        inner join TagData td5 on td5.Id = t.LatestRevisionId
        }
        where 1 = 1
        $?($$area$$ == 'someGuid'){
        and td2.AreaId = $$area$$
        }$:{
        and 3 = 3
        }
        and 2 = 2"

    let sample4 = "
        select * 
        from Tag t
        $?($$nestableCondition$$ != null) {
        inner join TagData td on td.Id = t.LatestRevisionId
            $?($$nestedCondition$$ != null) {
            inner join TagData td2 on td2.Id = t.LatestRevisionId
            }$:{
            inner join TagData td3 on td3.Id = t.LatestRevisionId
            }
        inner join TagData td4 on td4.Id = t.LatestRevisionId
            $?($$nestedCondition$$ != null) {
            inner join TagData td5 on td5.Id = t.LatestRevisionId
            }$:{
            inner join TagData td6 on td6.Id = t.LatestRevisionId
            }
        inner join TagData td7 on td7.Id = t.LatestRevisionId
        }$:{
        inner join TagData td8 on td8.Id = t.LatestRevisionId
        }
        inner join TagData td9 on td9.Id = t.LatestRevisionId
        $?($$nestableCondition$$ != null) {
        inner join TagData td10 on td10.Id = t.LatestRevisionId
            $?($$nestedCondition$$ != null) {
            inner join TagData td11 on td11.Id = t.LatestRevisionId
                $?($$nestedCondition$$ != null) {
                inner join TagData td12 on td12.Id = t.LatestRevisionId
                }$:{
                inner join TagData td13 on td13.Id = t.LatestRevisionId
                }
            }$:{
            inner join TagData td14 on td14.Id = t.LatestRevisionId
            }
        inner join TagData td15 on td15.Id = t.LatestRevisionId
        }$:{
        inner join TagData td16 on td16.Id = t.LatestRevisionId
        }
        where 1 = 1"

    let sample5 = "
        select * 
        from Tag t
        $?($$area[]$$ != null) {
        inner join TagData td on td.Id = t.LatestRevisionId
        }
        where 1 = 1
        $?($$area[]$$ != null){
        and td2.AreaId in ($$area[]$$)
        }
        and 2 = 2"
                
    [<Fact>]
    let ``when parsing sql with valid if expressions.`` ()=
        let context = Map.empty.Add("$$area$$", "someGuid").Add("$$subsystem$$", "someGuid2")
        let sql = sample
        let expectedSqlOutput = "
            select * 
            from Tag t 
            inner join TagData td2 on td2.Id = t.LatestRevisionId 
            inner join Area a on a.Id = td2.AreaId 
            where 1 = 1 and td2.AreaId = @p1 and td2.SubsystemId = @p2
            and 2 = 2"
        let ``expected sql output`` = expectedSqlOutput.RemoveWhiteSpace()
        
        let result = ``construct query`` sql context
         
        let ``actual sql output`` = result.Sql.RemoveWhiteSpace()

        ``actual sql output`` |> should equal ``expected sql output``

        result.Parameters.IsEmpty |> should equal false

    [<Fact>]
    let ``when parsing sql with valid else expressions.`` ()=
        let context = Map.empty.Add("$$area$$", "someGuid2")
        let sql = sample2
        let expectedSqlOutput = "
            select * 
            from Tag t 
            inner join TagData td3 on td3.Id = t.LatestRevisionId 
            where 1 = 1 
            and 2 = 2"
        let ``expected sql output`` = expectedSqlOutput.RemoveWhiteSpace()

        let result = ``construct query`` sql context 
        let ``actual sql output`` = result.Sql.RemoveWhiteSpace()

        ``actual sql output`` |> should equal ``expected sql output``
        result.Parameters.IsEmpty |> should equal true

    [<Fact>]
    let ``when parsing sql with multiple valid if and else expressions.`` ()=
        let context = Map.empty.Add("$$area$$", "someGuid2")
        let sql = sample3
        let expectedSqlOutput = "
            select * 
            from Tag t
            inner join TagData td3 on td3.Id = t.LatestRevisionId
            inner join TagData td4 on td4.Id = t.LatestRevisionId
            inner join Area a2 on a2.Id = td4.AreaId
            where 1 = 1
            and 3 = 3
            and 2 = 2"            
        let ``expected sql output`` = expectedSqlOutput.RemoveWhiteSpace()

        let result = ``construct query`` sql context 
        let ``actual sql output`` = result.Sql.RemoveWhiteSpace()

        ``actual sql output`` |> should equal ``expected sql output``
        result.Parameters.IsEmpty |> should equal true

    [<Fact>]
    let ``when parsing sql with nested valid if else statements`` ()=
        let context = Map.empty.Add("$$nestableCondition$$", "someValue").Add("$$nestedCondition$$", "someValue")
        let sql = sample4
        let expectedSqlOutput = "
            select * 
            from Tag t
            inner join TagData td on td.Id = t.LatestRevisionId
            inner join TagData td2 on td2.Id = t.LatestRevisionId
            inner join TagData td4 on td4.Id = t.LatestRevisionId
            inner join TagData td5 on td5.Id = t.LatestRevisionId
            inner join TagData td7 on td7.Id = t.LatestRevisionId
            inner join TagData td9 on td9.Id = t.LatestRevisionId
            inner join TagData td10 on td10.Id = t.LatestRevisionId
            inner join TagData td11 on td11.Id = t.LatestRevisionId
            inner join TagData td12 on td12.Id = t.LatestRevisionId
            inner join TagData td15 on td15.Id = t.LatestRevisionId
            where 1 = 1"            
        let ``expected sql output`` = expectedSqlOutput.RemoveWhiteSpace()

        let result = ``construct query`` sql context 
        let ``actual sql output`` = result.Sql.RemoveWhiteSpace()

        ``actual sql output`` |> should equal ``expected sql output``
        result.Parameters.IsEmpty |> should equal true

    [<Fact>]
    let ``when parsing sql with an array argument`` ()=
        let context = Map.empty.Add("$$area[]$$", "someValue1, someValue2, someValue3")
        let sql = sample5
        let expectedSqlOutput = "
            select * 
            from Tag t
            inner join TagData td on td.Id = t.LatestRevisionId
            where 1 = 1
            and td2.AreaId in (@p1,@p2,@p3)
            and 2 = 2"            
        let ``expected sql output`` = expectedSqlOutput.RemoveWhiteSpace()

        let result = ``construct query`` sql context 
        let ``actual sql output`` = result.Sql.RemoveWhiteSpace()

        ``actual sql output`` |> should equal ``expected sql output``
        result.Parameters.IsEmpty |> should equal false
